#ifndef I2C_H
#define I2C_H

#include <stdint.h>
#include <stm32.h>
#include <gpio.h>

struct i2c_package;
typedef void (*i2c_callback)(struct i2c_package*);

//Contains all information about I2C request
struct i2c_package {
  uint8_t slave;
  uint8_t reg;
  uint8_t size;
  uint8_t* data;
  i2c_callback callback;
};

void i2c_configure();

void i2c_write_callback(uint8_t slave, uint8_t reg, uint8_t data_size, uint8_t* data, i2c_callback callback);
void i2c_write(uint8_t slave, uint8_t reg, uint8_t data_size, uint8_t* data);

void i2c_read_callback(uint8_t slave, uint8_t reg, uint8_t* data, i2c_callback callback);
void i2c_read(uint8_t slave, uint8_t reg, uint8_t* data);

#endif //I2C_H
