#include <stm32.h>
#include <gpio.h>
#include <delay.h>

#include "lcd.h"
#include "i2c.h"
#include "acc.h"

static volatile int8_t y_value = 0;
static volatile int8_t x_value = 0;
static uint8_t last_x = LCD_PIXEL_WIDTH/2;
static uint8_t last_y = LCD_PIXEL_HEIGHT/2;

static volatile uint8_t new_values = 1;

void set_values(int8_t x, int8_t y) {
  x_value = x;
  y_value = y;
  new_values = 1;
}

uint8_t getPos(int8_t delta, uint8_t MAX, uint8_t last) {
  if (last + delta < 0)
    return 0;
  else if (last + delta > MAX)
    return MAX;

  return last + delta;
}

uint8_t getPosX(int8_t delta) {
  return getPos(delta, LCD_PIXEL_WIDTH-1, last_x);
}

uint8_t getPosY(int8_t delta) {
  return getPos(delta, LCD_PIXEL_HEIGHT-1, last_y);
}

void rcc_configure() {
  RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;
  RCC->AHB1ENR |= RCC_AHB1ENR_GPIOBEN;
}

int main() {
  rcc_configure();
  i2c_configure();
  acc_configure(set_values);
  lcd_configure();

  while (1) { //LCD active loop
    if (new_values) {
      //X and Y exchanged for convinient usage
      int8_t delta_x = y_value/8;
      int8_t delta_y = x_value/8;

      uint8_t new_x = getPosX(delta_x);
      uint8_t new_y = getPosY(delta_y);

      lcd_clear_cross(last_x, last_y);
      lcd_show_cross(new_x, new_y);
      last_x = new_x;
      last_y = new_y;
      new_values = 0;
    }
  }

  return 0;
}
