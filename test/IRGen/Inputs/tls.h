#include "shims/SwiftStdint.h"

static inline __swift_uint32_t _swift_stdlib_gettid() {
  static __thread __swift_uint32_t tid = 0;

  return tid;
}
