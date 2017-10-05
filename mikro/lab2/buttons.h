#ifndef __BUTTONS_H__
#define __BUTTONS_H__

#include "gpiodevice.h"

typedef struct {
  GPIODevice device;
  const char* name;
  const unsigned int nameLen;
  unsigned char wasPressed;
} Button;

#define Button_Init(gpio, pin, active_voltage, name, nameLen) \
  { GPIODevice_Init(gpio, pin, active_voltage), name, nameLen, 0 }

#define Button_New(varName, gpio, pin, active_voltage, name, nameLen) \
  Button varName = Button_Init(gpio, pin, active_voltage, name, nameLen)

#endif
