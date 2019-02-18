#include <stdbool.h>

static inline bool isRunningOnFairlyRecentOS() {
  if (__builtin_available(macOS 10.12, iOS 10, tvOS 10, watchOS 3, *)) {
    return true;
  } else {
    return false;
  }
}