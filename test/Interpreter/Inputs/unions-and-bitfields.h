#include <stdint.h>
#include <string.h>

union PlainUnion {
  uint32_t whole;
  unsigned char first;
};

struct PlainBitfield {
  uint32_t offset;
  uint32_t first: 8;
  uint32_t : 0;
};
_Static_assert(sizeof(struct PlainBitfield) == sizeof(uint64_t),
               "must fit in 64 bits");

struct PlainIndirect {
  uint32_t offset;
  struct {
    uint32_t whole;
  };
};

union BitfieldUnion {
  uint32_t whole;
  uint32_t first: 8;
};

struct BitfieldIndirect {
  uint32_t offset;
  struct {
    uint32_t first: 8;
    uint32_t : 0;
  };
};

struct UnionIndirect {
  uint32_t offset;
  union {
    uint32_t whole;
    unsigned char first;
  };
};

struct BitfieldUnionIndirect {
  uint32_t offset;
  union {
    uint32_t whole;
    uint32_t first: 8;
  };
};

static void populate(void *memory) {
  const uint32_t value = 0x11223344;
  memcpy(memory, &value, sizeof(value));
}

static void populateAtOffset(void *memory) {
  const uint32_t value = 0x11223344;
  memcpy((char *)memory + sizeof(uint32_t), &value, sizeof(value));
}

