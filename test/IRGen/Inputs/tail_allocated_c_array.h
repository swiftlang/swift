#include <stdint.h>

typedef struct foo {
  uint8_t a;
  uint16_t b;
  uint8_t  tailallocatedarray[0];
} foo;
