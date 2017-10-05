#include "acc.h"
#include "i2c.h"

#define NULL 0
#define LIS35DE_ADDR 0x1C
#define OUT_X 0x29
#define OUT_Y 0x2B
#define OUT_Z 0x2D
#define CTRL_REG_1 0x20
#define CTRL_REG_2 0x21
#define CTRL_REG_3 0x22
#define ACC_INT1_PIN_NR 1

static struct {
  int8_t x;
  int8_t y;
} acc_regs;

static volatile uint8_t is_acc_configured = 0;

static void acc_configuration_finished(struct i2c_package* unused) {
  is_acc_configured = 1;
}

static void acc_i2c_write_callback(uint8_t reg, uint8_t size, uint8_t* data, i2c_callback callback) {
  i2c_write_callback(LIS35DE_ADDR, reg, size, data, callback);
}

static void acc_i2c_read_callback(uint8_t reg, uint8_t size, uint8_t* data, i2c_callback callback) {
  i2c_read_callback(LIS35DE_ADDR, reg, data, callback);
}

static void acc_i2c_write(uint8_t reg, uint8_t size, uint8_t* data) {
  i2c_write(LIS35DE_ADDR, reg, size, data);
}

static AccCallback acc_callback;

#define CTRL_REG_1_PD 0x40U //Power down
#define CTRL_REG_1_X  0x01U
#define CTRL_REG_1_Y  0x02U
#define CTRL_REG_3_DR 0x04U //Data ready

void acc_configure(AccCallback callback) {
  acc_callback = callback;
  uint8_t zero = 0;
  uint8_t CTRL_REG_1_VALUE = CTRL_REG_1_PD | CTRL_REG_1_X | CTRL_REG_1_Y;
  uint8_t CTRL_REG_3_VALUE = CTRL_REG_3_DR; //Data ready event on INT1 line //
  acc_i2c_write(CTRL_REG_1, 1, &zero);
  acc_i2c_write(CTRL_REG_2, 1, &zero);
  acc_i2c_write_callback(CTRL_REG_3, 1, &zero, acc_configuration_finished);

  while (!is_acc_configured); //Wait until values are reset
  is_acc_configured = 0;

  GPIOinConfigure(GPIOA, ACC_INT1_PIN_NR, GPIO_PuPd_DOWN, EXTI_Mode_Interrupt, EXTI_Trigger_Rising_Falling);

  NVIC_EnableIRQ(EXTI1_IRQn);

  acc_i2c_write(CTRL_REG_1, 1, &CTRL_REG_1_VALUE);
  acc_i2c_write_callback(CTRL_REG_3, 1, &CTRL_REG_3_VALUE, acc_configuration_finished);

  while (!is_acc_configured); //Wait until configuration is finished.
}

static void read_x_value();

static void callback(struct i2c_package* ignored) {
  if (GPIOA->IDR & (1 << ACC_INT1_PIN_NR))
    read_x_value();
  else
    acc_callback(acc_regs.x, acc_regs.y);
}

static void read_y_value(struct i2c_package* ignored) {
  acc_i2c_read_callback(OUT_Y, 1, (uint8_t*)&acc_regs.y, callback);
}

static void read_x_value() {
  acc_i2c_read_callback(OUT_X, 1, (uint8_t*)&acc_regs.x, read_y_value);
}

void EXTI1_IRQHandler() { //Accelerometer data avaliable
  EXTI->PR = 1 << ACC_INT1_PIN_NR;
  read_x_value();
}
