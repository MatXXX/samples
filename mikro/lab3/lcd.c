#include "lcd_board_def.h"

#include <stdint.h>
#include <stm32.h>
#include <delay.h>
#include <gpio.h>
#include "lcd.h"

#define GPIO_LCD_CS   xcat(GPIO, LCD_CS_GPIO_N)
#define GPIO_LCD_A0   xcat(GPIO, LCD_A0_GPIO_N)
#define GPIO_LCD_SDA  xcat(GPIO, LCD_SDA_GPIO_N)
#define GPIO_LCD_SCK  xcat(GPIO, LCD_SCK_GPIO_N)

#define PIN_LCD_CS    (1U << LCD_CS_PIN_N)
#define PIN_LCD_A0    (1U << LCD_A0_PIN_N)
#define PIN_LCD_SDA   (1U << LCD_SDA_PIN_N)
#define PIN_LCD_SCK   (1U << LCD_SCK_PIN_N)

#define RCC_LCD_CS    xcat3(RCC_AHB1ENR_GPIO, LCD_CS_GPIO_N, EN)
#define RCC_LCD_A0    xcat3(RCC_AHB1ENR_GPIO, LCD_A0_GPIO_N, EN)
#define RCC_LCD_SDA   xcat3(RCC_AHB1ENR_GPIO, LCD_SDA_GPIO_N, EN)

#define RCC_LCD_SCK   xcat3(RCC_AHB1ENR_GPIO, LCD_SCK_GPIO_N, EN)

#define LCD_COLOR_WHITE    0xFFFF
#define LCD_COLOR_BLUE     0x001F

/* Needed delay(s)  */

#define Tinit   150
#define T120ms  (MAIN_CLOCK_MHZ * 120000 / 4)

/* Text mode globals */

static uint16_t lcd_main_color = LCD_COLOR_BLUE;
static uint16_t lcd_back_color = LCD_COLOR_WHITE;

static void CS(uint32_t bit) {
  if (bit) {
    GPIO_LCD_CS->BSRRL = PIN_LCD_CS; /* Activate chip select line. */
  }
  else {
    GPIO_LCD_CS->BSRRH = PIN_LCD_CS; /* Deactivate chip select line. */
  }
}

static void A0(uint32_t bit) {
  if (bit) {
    GPIO_LCD_A0->BSRRL = PIN_LCD_A0; /* Set data/command line to data. */
  }
  else {
    GPIO_LCD_A0->BSRRH = PIN_LCD_A0; /* Set data/command line to command. */
  }
}

static void SDA(uint32_t bit) {
  if (bit) {
    GPIO_LCD_SDA->BSRRL = PIN_LCD_SDA; /* Set data bit one. */
  }
  else {
    GPIO_LCD_SDA->BSRRH = PIN_LCD_SDA; /* Set data bit zero. */
  }
}

static void SCK(uint32_t bit) {
  if (bit) {
    GPIO_LCD_SCK->BSRRL = PIN_LCD_SCK; /* Rising clock edge. */
  }
  else {
    GPIO_LCD_SCK->BSRRH = PIN_LCD_SCK; /* Falling clock edge. */
  }
}


static void lcd_write_serial(uint32_t data, uint32_t length) {
  uint32_t mask;

  mask = 1U << (length - 1);
  while (length > 0) {
    SDA(data & mask); // Set bit
    --length;         // Add some delay
    SCK(1);           // Rising edge writes bit
    mask >>= 1;       // Add some delay
    SCK(0);           // Falling edge ends the bit transmission
  }
}

static void lcd_write_command(uint32_t data) {
  A0(0);
  lcd_write_serial(data, 8);
  A0(1);
}

static void lcd_write_data_8(uint32_t data) {
  /* A0(1); is already set */
  lcd_write_serial(data, 8);
}

static void lcd_write_data_16(uint32_t data) {
  /* A0(1); is already set */
  lcd_write_serial(data, 16);
}

static void lcd_write_data_24(uint32_t data) {
  /* A0(1); is already set */
  lcd_write_serial(data, 24);
}

static void lcd_write_data_32(uint32_t data) {
  /* A0(1); is already set */
  lcd_write_serial(data, 32);
}


void lcd_set_rectangle(uint16_t x1, uint16_t y1, uint16_t x2, uint16_t y2) {
  lcd_write_command(0x2A);
  lcd_write_data_16(x1);
  lcd_write_data_16(x2);

  lcd_write_command(0x2B);
  lcd_write_data_16(y1);
  lcd_write_data_16(y2);

  lcd_write_command(0x2C);
}

void lcd_clear() {
  int i, j;

  CS(0);
  lcd_set_rectangle(0, 0, LCD_PIXEL_WIDTH - 1, LCD_PIXEL_HEIGHT - 1);
  for (j = 0; j < LCD_PIXEL_HEIGHT; ++j) {
    for (i = 0; i < LCD_PIXEL_WIDTH; ++i) {
      lcd_write_data_16(lcd_back_color);
    }
  }
  CS(1);
}

void lcd_clear_cross(uint8_t last_x, uint8_t last_y) {
  uint8_t i, j;
  CS(0);
  //We don't care about edge cases here, we are clearing the screen anyways
  lcd_set_rectangle(last_x - CROSS_ARM, last_y - CROSS_ARM, last_x + CROSS_ARM, last_y + CROSS_ARM);
  for (i = 0; i < 2 * CROSS_ARM + 1; ++i) {
    for (j = 0; j < 2 * CROSS_ARM + 1; ++j) {
      lcd_write_data_16(lcd_back_color);
    }
  }
  CS(1);
}

struct lcd_cross_boundaries {
  uint8_t iter_start;
  uint8_t iter_end;
  uint8_t coord_min;
  uint8_t coord_max;
};

static struct lcd_cross_boundaries lcd_calculate_boundaries(uint8_t coord, uint8_t max) {
  struct lcd_cross_boundaries result = { 0, 2 * CROSS_ARM + 1, coord - CROSS_ARM, coord + CROSS_ARM };

  if (coord < CROSS_ARM) {
    result.iter_start = CROSS_ARM - coord;
    result.iter_end = 2 * CROSS_ARM + 1;
    result.coord_min = 0;
    result.coord_max = coord + CROSS_ARM;
  }
  else if (coord + CROSS_ARM >= max) {
    result.iter_start = 0;
    result.iter_end = CROSS_ARM + max - coord;
    result.coord_min = coord - CROSS_ARM;
    result.coord_max = max - 1;
  }

  return result;
}

void lcd_show_cross(uint8_t x_coord, uint8_t y_coord) {
  int i, j;
  //Setting boundaries of drawing rectangle.
  struct lcd_cross_boundaries x = lcd_calculate_boundaries(x_coord, LCD_PIXEL_WIDTH);
  struct lcd_cross_boundaries y = lcd_calculate_boundaries(y_coord, LCD_PIXEL_HEIGHT);

  CS(0);
  lcd_set_rectangle(x.coord_min, y.coord_min, x.coord_max, y.coord_max);
  for (j = y.iter_start; j < y.iter_end; ++j) {
    for (i = x.iter_start; i < x.iter_end; ++i) {
      if (i == CROSS_ARM || j == CROSS_ARM)
        lcd_write_data_16(lcd_main_color);
      else
        lcd_write_data_16(lcd_back_color);
    }
  }
  CS(1);
}

static void lcd_controller_configure(void) {
  /* Activate chip select */
  CS(0);

  Delay(Tinit);

  /* Sleep out */
  lcd_write_command(0x11);

  Delay(T120ms);

  /* Frame rate */
  lcd_write_command(0xB1);
  lcd_write_data_24(0x023C3C);
  lcd_write_command(0xB2);
  lcd_write_data_24(0x053C3C);
  lcd_write_command(0xB3);
  lcd_write_data_24(0x053C3C);
  lcd_write_data_24(0x053C3C);

  /* Dot inversion */
  lcd_write_command(0xB4);
  lcd_write_data_8(0x03);

  /* Power sequence */
  lcd_write_command(0xC0);
  lcd_write_data_24(0x280804);
  lcd_write_command(0xC1);
  lcd_write_data_8(0xC0);
  lcd_write_command(0xC2);
  lcd_write_data_16(0x0D00);
  lcd_write_command(0xC3);
  lcd_write_data_16(0x8D2A);
  lcd_write_command(0xC4);
  lcd_write_data_16(0x8DEE);

  /* VCOM */
  lcd_write_command(0xC5);
  lcd_write_data_8(0x1A);
  /* Memory and color write direction */
  lcd_write_command(0x36);
  lcd_write_data_8(0xC0);

  /* Color mode 16 bit per pixel */
  lcd_write_command(0x3A);
  lcd_write_data_8(0x05);

  /* Gamma sequence */
  lcd_write_command(0xE0);
  lcd_write_data_32(0x0422070A);
  lcd_write_data_32(0x2E30252A);
  lcd_write_data_32(0x28262E3A);
  lcd_write_data_32(0x00010313);
  lcd_write_command(0xE1);
  lcd_write_data_32(0x0416060D);
  lcd_write_data_32(0x2D262327);
  lcd_write_data_32(0x27252D3B);
  lcd_write_data_32(0x00010413);

  /* Display on */
  lcd_write_command(0x29);

  /* Deactivate chip select */
  CS(1);
}

static void lcd_set_colors(uint16_t text, uint16_t back) {
  lcd_main_color = text;
  lcd_back_color = back;
}

void lcd_configure() {
  RCC->AHB1ENR |= RCC_LCD_CS | RCC_LCD_A0 | RCC_LCD_SDA | RCC_LCD_SCK;

  __NOP();
  __NOP();
  /* Initialize global variables. */
  lcd_set_colors(LCD_COLOR_WHITE, LCD_COLOR_BLUE);
  /* Initialize hardware. */
  CS(1); /* Set CS inactive. */
  GPIOoutConfigure(GPIO_LCD_CS, LCD_CS_PIN_N, GPIO_OType_PP,
                   GPIO_High_Speed, GPIO_PuPd_NOPULL);

  A0(1); /* Data are sent default. */
  GPIOoutConfigure(GPIO_LCD_A0, LCD_A0_PIN_N, GPIO_OType_PP,
                   GPIO_High_Speed, GPIO_PuPd_NOPULL);

  SDA(0);
  GPIOoutConfigure(GPIO_LCD_SDA, LCD_SDA_PIN_N, GPIO_OType_PP,
                   GPIO_High_Speed, GPIO_PuPd_NOPULL);

  SCK(0); /* Data bit is written on rising clock edge. */
  GPIOoutConfigure(GPIO_LCD_SCK, LCD_SCK_PIN_N, GPIO_OType_PP,
                   GPIO_High_Speed, GPIO_PuPd_NOPULL);

  lcd_controller_configure();
  lcd_clear();
}
