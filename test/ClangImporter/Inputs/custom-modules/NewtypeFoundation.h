// Test header for swift_newtype with Foundation types

#include <stdint.h>

// Test swift_newtype with integer types on all platforms
typedef uint32_t MyUInt32Newtype __attribute__((swift_newtype(struct)));
