
#include <stm32.h>
#include <gpio.h>
#include <delay.h>

#include "leds.h"
#include "fifo.h"
#include "usart.h"
#include "buttons.h"

#define LEDS_COUNT 4
GPIODevice_New(RED_LED, RED_LED_GPIO, RED_LED_PIN, LOW_VOLTAGE);
GPIODevice_New(BLUE_LED, BLUE_LED_GPIO, BLUE_LED_PIN, LOW_VOLTAGE);
GPIODevice_New(GREEN_LED, GREEN_LED_GPIO, GREEN_LED_PIN, LOW_VOLTAGE);
GPIODevice_New(GREEN2_LED, GREEN2_LED_GPIO, GREEN2_LED_PIN, HIGH_VOLTAGE);
GPIODevice* LEDS[LEDS_COUNT] = { &RED_LED, &GREEN_LED, &BLUE_LED, &GREEN2_LED };

#define BUTTON_COUNT 7
Button BUTTONS[BUTTON_COUNT] =
  { Button_Init(GPIOC, 13, LOW_VOLTAGE,  "USER",    4),
    Button_Init(GPIOB, 3,  LOW_VOLTAGE,  "LEFT",    4),
    Button_Init(GPIOB, 4,  LOW_VOLTAGE,  "RIGHT",   5),
    Button_Init(GPIOB, 5,  LOW_VOLTAGE,  "UP",      2),
    Button_Init(GPIOB, 6,  LOW_VOLTAGE,  "DOWN",    4),
    Button_Init(GPIOB, 10, LOW_VOLTAGE,  "FIRE",    4),
    Button_Init(GPIOA, 0,  HIGH_VOLTAGE, "AT MODE", 7) };

typedef enum {
  NONE = 0,
  CMD_ON,
  CMD_OFF,
  CMD_TOGGLE
} COMMAND;

COMMAND getCommand(char c) {
  switch (c) {
    case 'n':
      return CMD_ON;
    case 'f':
      return CMD_OFF;
    case 't':
      return CMD_TOGGLE;
  }
  return NONE;
}

typedef void (*GIPOFunctionPtr)(GPIODevice*);
GIPOFunctionPtr COMMANDS[] = { GPIODevice_On, GPIODevice_Off, GPIODevice_Toggle };

void ledCommand(unsigned char ledId, COMMAND cmd) {
  if(ledId == 0 || ledId > LEDS_COUNT || cmd == NONE)
    return;

  (*COMMANDS[cmd - 1])(LEDS[ledId - 1]);
}

int main() {
  int i = 0;
  int buttonsPressed = 0;
  char command[] = {'\0', '\0'};
  int commandIndex = 0;

  RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN |
                  RCC_AHB1ENR_GPIOBEN |
                  RCC_AHB1ENR_GPIOCEN;

  RCC->APB1ENR |= RCC_APB1ENR_USART2EN;

  __NOP();
  GPIODevice_Off(&RED_LED);
  GPIODevice_Off(&BLUE_LED);
  GPIODevice_Off(&GREEN_LED);
  GPIODevice_Off(&GREEN2_LED);

  LED_Configure();
  USART_Configure();

  FIFO txFifo;
  FIFO_Init(&txFifo);

  for(;;) {
    if(USART_Read(&command[commandIndex])) {
      commandIndex++;
      if(commandIndex == 2) {
        ledCommand(command[0] - '0', getCommand(command[1]));
        commandIndex = 0;
      }
    }

    if(!FIFO_Empty(&txFifo) && USART_CanSend())
        USART_Send(FIFO_Read(&txFifo));

    for(i = 0, buttonsPressed = 0; i < BUTTON_COUNT; i++) {
      if(!BUTTONS[i].wasPressed && GPIODevice_GetState(&BUTTONS[i].device)) {
        BUTTONS[i].wasPressed = 1;
        FIFO_WriteStr(&txFifo, BUTTONS[i].name, BUTTONS[i].nameLen);
        FIFO_WriteStr(&txFifo, "\n\r", 2);
        buttonsPressed = 1;
      }
    }

    if(buttonsPressed) { //Prevents bouncing (does it?)
      Delay(10000);
      buttonsPressed = 0;
    }

    for(i = 0; i < BUTTON_COUNT; i++) {
      if(BUTTONS[i].wasPressed && !GPIODevice_GetState(&BUTTONS[i].device))
        BUTTONS[i].wasPressed = 0;
    }
  }
}
