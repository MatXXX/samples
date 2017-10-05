#ifndef __GPIODEVICE_H__
#define __GPIODEVICE_H__
typedef enum {
    LOW_VOLTAGE = 0,
    HIGH_VOLTAGE = 1
} ACTIVE_VOLTAGE;

typedef struct {
    GPIO_TypeDef*  gpio;
    const unsigned short  pin_mask;
    const ACTIVE_VOLTAGE active_voltage;
} GPIODevice;

void GPIODevice_SetHigh(GPIODevice* device) {
    device->gpio->BSRRL = device->pin_mask;
}

void GPIODevice_SetLow(GPIODevice* device) {
    device->gpio->BSRRH = device->pin_mask;
}

void GPIODevice_On(GPIODevice* device) {
    if(device->active_voltage == HIGH_VOLTAGE)
        GPIODevice_SetHigh(device);
    else
        GPIODevice_SetLow(device);
}

void GPIODevice_Off(GPIODevice* device) {
    if(device->active_voltage == LOW_VOLTAGE)
        GPIODevice_SetHigh(device);
    else
        GPIODevice_SetLow(device);
}

ACTIVE_VOLTAGE GPIODevice_GetVoltage(GPIODevice* device) {
    return (device->gpio->IDR & device->pin_mask) != 0;
}

unsigned char GPIODevice_GetState(GPIODevice* device) {
    return GPIODevice_GetVoltage(device) == device->active_voltage;
}

void GPIODevice_Toggle(GPIODevice* device) {
    if(GPIODevice_GetState(device))
        GPIODevice_Off(device);
    else
        GPIODevice_On(device);
}

#define GPIODevice_Init(gpio, pin, activeVoltage) \
    { gpio, 1U<<pin, activeVoltage }

#define GPIODevice_New(name, gpio, pin, activeVoltage) \
    GPIODevice name = GPIODevice_Init(gpio, pin, activeVoltage)

#endif //__GPIODEVICE_H__
