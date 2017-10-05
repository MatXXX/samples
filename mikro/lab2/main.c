#include <stm32.h>
#include <gpio.h>
#include <delay.h>

#include "fifo.h"
#include "usart.h"
#include "buttons.h"
#include "gpiodevice.h"
#include "leds.h"

Button_New(USER_BUTTON,  GPIOC, 13, LOW_VOLTAGE,  "USER",  4);
Button_New(LEFT_BUTTON,  GPIOB, 3,  LOW_VOLTAGE,  "LEFT",  4);
Button_New(RIGHT_BUTTON, GPIOB, 4,  LOW_VOLTAGE,  "RIGHT", 5);
Button_New(UP_BUTTON,    GPIOB, 5,  LOW_VOLTAGE,  "UP",    2);
Button_New(DOWN_BUTTON,  GPIOB, 6,  LOW_VOLTAGE,  "DOWN",  4);
Button_New(FIRE_BUTTON,  GPIOB, 10, LOW_VOLTAGE,  "FIRE",  4);
Button_New(MODE_BUTTON,  GPIOA, 0,  HIGH_VOLTAGE, "MODE",  4);

Fifo_CHAR_PTR_New(MSGS_FIFO);
Fifo_UINT_New(LENGTHS_FIFO);


void HandleButtonInterruption(Button* button) {
  Fifo_CHAR_PTR_Add(&MSGS_FIFO, button->name);
  Fifo_UINT_Add(&LENGTHS_FIFO, button->nameLen);
  if(GPIODevice_GetState(&button->device)) {
    Fifo_CHAR_PTR_Add(&MSGS_FIFO, " DOWN\r\n");
    Fifo_UINT_Add(&LENGTHS_FIFO, 7);
  } else {
    Fifo_CHAR_PTR_Add(&MSGS_FIFO, " UP\r\n");
    Fifo_UINT_Add(&LENGTHS_FIFO, 5);
  }
}

void SendMessage() {
  if(!(DMA1_Stream6->CR & DMA_SxCR_EN) && !Fifo_CHAR_PTR_Empty(&MSGS_FIFO)) {
    DMA1_Stream6->M0AR = (uint32_t)Fifo_CHAR_PTR_Get(&MSGS_FIFO);
    DMA1_Stream6->NDTR = Fifo_UINT_Get(&LENGTHS_FIFO);
    DMA1_Stream6->CR |= DMA_SxCR_EN;
  }
}



void EXTI15_10_IRQHandler(void) {
  uint32_t pr = EXTI->PR;
  if(pr & EXTI_PR_PR13) {
    EXTI->PR = EXTI_PR_PR13;
    HandleButtonInterruption(&USER_BUTTON);
  }

  if(pr & EXTI_PR_PR10) {
    EXTI->PR = EXTI_PR_PR10;
    HandleButtonInterruption(&FIRE_BUTTON);
  }
  SendMessage();
}

void EXTI9_5_IRQHandler(void) {
  uint32_t pr = EXTI->PR;
  if(pr & EXTI_PR_PR6) {
    EXTI->PR = EXTI_PR_PR6;
    HandleButtonInterruption(&DOWN_BUTTON);
  }

  if(pr & EXTI_PR_PR5) {
    EXTI->PR = EXTI_PR_PR5;
    HandleButtonInterruption(&UP_BUTTON);
  }
  SendMessage();
}

void EXTI4_IRQHandler(void) {
  uint32_t pr = EXTI->PR;
  if(pr & EXTI_PR_PR4) {
    EXTI->PR = EXTI_PR_PR4;
    HandleButtonInterruption(&RIGHT_BUTTON);
    SendMessage();
  }
}

void EXTI3_IRQHandler(void) {
  uint32_t pr = EXTI->PR;
  if(pr & EXTI_PR_PR3) {
    EXTI->PR = EXTI_PR_PR3;
    HandleButtonInterruption(&LEFT_BUTTON);
    SendMessage();
  }
}

void EXTI0_IRQHandler(void) {
  uint32_t pr = EXTI->PR;
  if(pr & EXTI_PR_PR0) {
    EXTI->PR = EXTI_PR_PR0;
    HandleButtonInterruption(&MODE_BUTTON);
    SendMessage();
  }
}

void DMA1_Stream6_IRQHandler(void) {
  uint32_t isr = DMA1->HISR;
  if(isr & DMA_HISR_TCIF6) {
    DMA1->HIFCR = DMA_HIFCR_CTCIF6;
    SendMessage();
  }
}

#define CONFIG_INTERRUPTION(EXTICRID, PIN, GPIOCHAR) \
  SYSCFG->EXTICR[EXTICRID-1] |= SYSCFG_EXTICR##EXTICRID##_EXTI##PIN##_P##GPIOCHAR; \
  EXTI->FTSR |= 1U << PIN; \
  EXTI->RTSR |= 1U << PIN; \
  EXTI->IMR  |= 1U << PIN

void INTERRUPTIONS_Configure() {
  DMA1_Stream6->CR = 4U << 25 |
                    DMA_SxCR_PL_1 |
                    DMA_SxCR_MINC |
                    DMA_SxCR_DIR_0 |
                    DMA_SxCR_TCIE;

  DMA1_Stream6->PAR = (uint32_t)&USART2->DR;

  DMA1->HIFCR = DMA_HIFCR_CTCIF6;

  NVIC_EnableIRQ(DMA1_Stream6_IRQn);

  NVIC_EnableIRQ(EXTI15_10_IRQn);
  CONFIG_INTERRUPTION(4, 13, C);
  CONFIG_INTERRUPTION(3, 10, B);

  NVIC_EnableIRQ(EXTI9_5_IRQn);
  CONFIG_INTERRUPTION(2, 6, B);
  CONFIG_INTERRUPTION(2, 5, B);

  NVIC_EnableIRQ(EXTI4_IRQn);
  CONFIG_INTERRUPTION(2, 4, B);

  NVIC_EnableIRQ(EXTI3_IRQn);
  CONFIG_INTERRUPTION(1, 3, B);

  NVIC_EnableIRQ(EXTI0_IRQn);
  CONFIG_INTERRUPTION(1, 0, A);
}

int main() {
  RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN |
                  RCC_AHB1ENR_GPIOBEN |
                  RCC_AHB1ENR_GPIOCEN |
                  RCC_AHB1ENR_DMA1EN;
  RCC->APB1ENR |= RCC_APB1ENR_USART2EN;
  RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;

  __NOP();

  USART_Configure();
  INTERRUPTIONS_Configure();

  return 0;
}
