#include <stdint.h>

#define CREATE_ARRAY(T, N)                                                     \
  struct {                                                                     \
    T data[N];                                                                 \
    uint64_t size;                                                             \
  }

typedef struct {
    int32_t a;
    double b[16];
} Thing;

typedef struct {
  uint64_t a;
  uint8_t b;
  CREATE_ARRAY(Thing, 16) c;
  uint32_t d;
  uint64_t e;
  uint64_t f;
  CREATE_ARRAY(uint32_t, 4) g;
  CREATE_ARRAY(uint64_t, 4) h;
  CREATE_ARRAY(uint64_t, 4) i;
} Thing2;

typedef struct {
  int64_t a;
  CREATE_ARRAY(Thing2, 512) c;
} Thing3;
