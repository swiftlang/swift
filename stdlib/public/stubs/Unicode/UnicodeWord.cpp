//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_ENABLE_UNICODE_DATA
#include "Common/WordData.h"
#else
#include "swift/Runtime/Debug.h"
#endif
#include "SwiftShims/UnicodeData.h"
#include <limits>

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint8_t _swift_stdlib_getWordBreakProperty(__swift_uint32_t scalar) {
#if !SWIFT_STDLIB_ENABLE_UNICODE_DATA
  swift::swift_abortDisabledUnicodeSupport();
#else
  auto low = 0;
  auto high = 1086 - 1;
  
  while (high >= low) {
    auto idx = low + (high - low) / 2;
    
    auto entry = _swift_stdlib_words[idx];
    
    // Shift the range count out of the value.
    auto lower = (entry << 11) >> 11;
    
    // Shift the enum out first, then shift out the scalar value.
    auto upper = lower + (entry >> 21) - 1;
    
    if (scalar >= lower && scalar <= upper) {
      return _swift_stdlib_words_data[idx];
    }
    
    if (scalar > upper) {
      low = idx + 1;
      continue;
    }
    
    if (scalar < lower) {
      high = idx - 1;
      continue;
    }
  }
  
  // If we made it out here, then our scalar was not found in the grapheme
  // array (this occurs when a scalar doesn't map to any grapheme break
  // property). Return the max value here to indicate .any.
  return std::numeric_limits<__swift_uint8_t>::max();
#endif
}
