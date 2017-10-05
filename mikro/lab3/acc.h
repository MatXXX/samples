#ifndef ACC_H
#define ACC_H

#include <stdint.h>
#include <stm32.h>
#include <gpio.h>

typedef void (*AccCallback) (int8_t, int8_t);

//configures accelerometer, callback will be called after
//data from device is received
void acc_configure(AccCallback callback);

#endif //ACC_H
