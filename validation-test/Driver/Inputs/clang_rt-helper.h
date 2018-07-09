#include <stdbool.h>

static inline bool testNewOS() {
  if (__builtin_available(macOS 10.12, *)) {
    return true;
  } else {
    return false;
  }
}