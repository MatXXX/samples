#include <string.h>

#include "i2c.h"
#include "fifo.h"

#define I2C_SPEED_HZ 100000
#define PCLK1_MHZ 16

struct i2c_controller;
struct i2c_package;
static uint8_t i2c_is_active = 0;
typedef void (*i2c_function)(struct i2c_controller*);

//Structure used to control transmission of one i2c_package

enum i2c_write_state {
  I2C_WRITE_SEND_ADDRESS_STATE,
  I2C_WRITE_SEND_REGISTER_STATE,
  I2C_WRITE_TRANSMIT_STATE,
  I2C_WRITE_TRANSMISSTION_ENDED_STATE
};

enum i2c_read_state {
  I2C_READ_SEND_ADDRESS_STATE,
  I2C_READ_SEND_REGISTER_STATE,
  I2C_READ_SEND_START_AS_RECEIVER_STATE,
  I2C_READ_SEND_ADDRESS_AS_RECEIVER_STATE,
  I2C_READ_ON_ADDRESS_RECEIVED_STATE,
  I2C_READ_RECEIVE_DATA_STATE
};

struct i2c_controller {
  struct i2c_package package;
  uint8_t state;
  uint8_t data_processed;
  uint8_t is_reader;
};

FIFO_TYPE(i2c_controller, struct i2c_controller);

fifo_i2c_controller i2c_fifo;

static void i2c_start_transmission();

static void i2c_add_request(struct i2c_controller* controller) {
  __disable_irq();
  fifo_i2c_controller_add(&i2c_fifo, controller);
  __enable_irq();
  if (!i2c_is_active)
    i2c_start_transmission();
}

static void i2c_queue_element_handled() {
  fifo_i2c_controller_pop(&i2c_fifo);
  if (!fifo_i2c_controller_empty(&i2c_fifo))
    i2c_start_transmission();
  else
    i2c_is_active = 0;
}

static void i2c_write_package(struct i2c_package* data) {
  struct i2c_controller controller = { *data, I2C_WRITE_SEND_ADDRESS_STATE, 0, 0 };
  i2c_add_request(&controller);
}

void i2c_write_callback(uint8_t slave, uint8_t reg, uint8_t data_size, uint8_t* data, i2c_callback callback) {
  struct i2c_package package = { slave, reg, data_size, data, callback };
  i2c_write_package(&package);
}

void i2c_write(uint8_t slave, uint8_t reg, uint8_t data_size, uint8_t* data) {
  struct i2c_package package = { slave, reg, data_size, data, NULL };
  i2c_write_package(&package);
}


//Hardware related functions follow

void i2c_configure() {
  memset(&i2c_fifo, 0, sizeof(i2c_fifo));
  RCC->APB1ENR |= RCC_APB1ENR_I2C1EN;

  __NOP();

  GPIOafConfigure(GPIOB, 8, GPIO_OType_OD,
                  GPIO_Low_Speed, GPIO_PuPd_NOPULL,
                  GPIO_AF_I2C1);
  GPIOafConfigure(GPIOB, 9, GPIO_OType_OD,
                  GPIO_Low_Speed, GPIO_PuPd_NOPULL,
                  GPIO_AF_I2C1);
  I2C1->CR1 = 0;
  I2C1->CR2 = PCLK1_MHZ;
  I2C1->CCR = (PCLK1_MHZ * 1000000) / (I2C_SPEED_HZ << 1);
  I2C1->TRISE = PCLK1_MHZ + 1;

  I2C1->CR2 |=  I2C_CR2_ITEVTEN;
  I2C1->CR2 |=  I2C_CR2_ITBUFEN;

  NVIC_EnableIRQ(I2C1_EV_IRQn);

  I2C1->CR1 |= I2C_CR1_PE;
}

static void i2c_start_transmission() {
  i2c_is_active = 1;
  I2C1->CR1 |= I2C_CR1_START;
}

//Following functions are I2C write states
static void i2c_write_send_address_state(struct i2c_controller* controller) {
  controller->state = I2C_WRITE_SEND_REGISTER_STATE;
  I2C1->DR = controller->package.slave << 1;
}

static void i2c_write_send_register_state(struct i2c_controller* controller) {
  controller->state = I2C_WRITE_TRANSMIT_STATE;
  I2C1->SR2;
  I2C1->DR = controller->package.reg;
}

static void i2c_write_transmit_state(struct i2c_controller* controller) {
  controller->data_processed++;
  if (controller->data_processed >= controller->package.size) {
    controller->state = I2C_WRITE_TRANSMISSTION_ENDED_STATE;
  }
  I2C1->DR = controller->package.data[controller->data_processed - 1];
}

static void i2c_write_transmission_ended_state(struct i2c_controller* controller) {
  controller->state = I2C_WRITE_SEND_ADDRESS_STATE;
  I2C1->CR1 |= I2C_CR1_STOP;

  //Storing a copy of package for passing it later to callback.
  //i2c_queue_element_handled pops controller out of queue
  //so it wouldn't be safe to access it thereafter.
  struct i2c_package package = controller->package;
  i2c_queue_element_handled();

  if (controller->package.callback) {
    controller->package.callback(&package);
  }
}

static i2c_function i2c_write_states[] = {
  i2c_write_send_address_state,
  i2c_write_send_register_state,
  i2c_write_transmit_state,
  i2c_write_transmission_ended_state
};

static void i2c_read_package(struct i2c_package* data) {
  struct i2c_controller controller = { *data, I2C_READ_SEND_ADDRESS_STATE, 0, 1 };
  i2c_add_request(&controller);
}

void i2c_read_callback(uint8_t slave, uint8_t reg, uint8_t* data, i2c_callback callback) {
  struct i2c_package package = { slave, reg, 1, data, callback };
  i2c_read_package(&package);
}

void i2c_read(uint8_t slave, uint8_t reg, uint8_t* data) {
  struct i2c_package package = { slave, reg, 1, data, NULL };
  i2c_read_package(&package);
}

static void i2c_read_send_address_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_SEND_REGISTER_STATE;
  I2C1->DR = controller->package.slave << 1;
}

static void i2c_read_send_register_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_SEND_START_AS_RECEIVER_STATE;
  I2C1->SR2;
  I2C1->DR = controller->package.reg;
}

static void i2c_read_send_start_as_receiver_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_SEND_ADDRESS_AS_RECEIVER_STATE;
  I2C1->CR1 |= I2C_CR1_START;
}

static void i2c_read_send_address_as_receiver_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_ON_ADDRESS_RECEIVED_STATE;
  I2C1->DR = (controller->package.slave << 1) | 1U;
  I2C1->CR1 &= ~I2C_CR1_ACK;
}

static void i2c_read_on_address_received_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_RECEIVE_DATA_STATE;
  I2C1->SR2;
  I2C1->CR1 |= I2C_CR1_STOP;
}

static void i2c_read_receive_data_state(struct i2c_controller* controller) {
  controller->state = I2C_READ_SEND_ADDRESS_STATE;
  *(controller->package.data) = I2C1->DR;
  i2c_queue_element_handled();

  if (controller->package.callback)
    controller->package.callback(&(controller->package));
}

static i2c_function i2c_read_states[] = {
  i2c_read_send_address_state,
  i2c_read_send_register_state,
  i2c_read_send_start_as_receiver_state,
  i2c_read_send_address_as_receiver_state,
  i2c_read_on_address_received_state,
  i2c_read_receive_data_state
};

void I2C1_EV_IRQHandler() {
  uint32_t sr1 = I2C1->SR1;
  //If any unexpected event appears, discard it.
  if (fifo_i2c_controller_empty(&i2c_fifo))
    return;

  struct i2c_controller* controller = fifo_i2c_controller_peek(&i2c_fifo);
  if (controller->is_reader) {
    if ((sr1 & I2C_SR1_SB && controller->state == I2C_READ_SEND_ADDRESS_STATE) ||
        (sr1 & I2C_SR1_ADDR && controller->state == I2C_READ_SEND_REGISTER_STATE) ||
        (sr1 & I2C_SR1_BTF && controller->state == I2C_READ_SEND_START_AS_RECEIVER_STATE) ||
        (sr1 & I2C_SR1_SB && controller->state == I2C_READ_SEND_ADDRESS_AS_RECEIVER_STATE) ||
        (sr1 & I2C_SR1_ADDR && controller->state == I2C_READ_ON_ADDRESS_RECEIVED_STATE) ||
        (sr1 & I2C_SR1_RXNE && controller->state == I2C_READ_RECEIVE_DATA_STATE))
      i2c_read_states[controller->state](controller);
  }
  else {
    if ((sr1 & I2C_SR1_SB && controller->state == I2C_WRITE_SEND_ADDRESS_STATE) ||
        (sr1 & I2C_SR1_ADDR && controller->state == I2C_WRITE_SEND_REGISTER_STATE) ||
        (sr1 & I2C_SR1_TXE && controller->state == I2C_WRITE_TRANSMIT_STATE) ||
        (sr1 & I2C_SR1_BTF && controller->state == I2C_WRITE_TRANSMISSTION_ENDED_STATE))
      i2c_write_states[controller->state](controller);
  }
}
