#ifndef LCD_H
#define LCD_H

#define LCD_PIXEL_WIDTH   128
#define LCD_PIXEL_HEIGHT  160
#define CROSS_ARM 5

void lcd_clear_cross(uint8_t last_x, uint8_t last_y);
void lcd_show_cross(uint8_t x, uint8_t y);
void lcd_configure();

#endif //LCD_H
