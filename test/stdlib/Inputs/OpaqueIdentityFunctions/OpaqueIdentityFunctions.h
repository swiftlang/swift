#include <stdint.h>

// These functions are opaque to the Swift SIL-level optimizer.

int8_t  getInt8(int8_t x);
int16_t getInt16(int16_t x);
int32_t getInt32(int32_t x);
int64_t getInt64(int64_t x);

uint8_t  getUInt8(uint8_t x);
uint16_t getUInt16(uint16_t x);
uint32_t getUInt32(uint32_t x);
uint64_t getUInt64(uint64_t x);

