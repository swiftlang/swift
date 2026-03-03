//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_ENABLE_UNICODE_DATA
#include "Common/EmojiData.h"
#else
#include "swift/Runtime/Debug.h"
#endif

#include "UnicodeData.h"
#include <stdint.h>

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_bool _swift_stdlib_isExtendedPictographic(__swift_uint32_t scalar) {
  // Fast Path: U+A9 is the first scalar to be an extended pictographic.
  if (scalar < 0xA9) {
    return false;
  }

#if !SWIFT_STDLIB_ENABLE_UNICODE_DATA
  swift::swift_abortDisabledUnicodeSupport();
#else
  auto dataIdx = _swift_stdlib_getScalarBitArrayIdx(scalar,
                                                    _swift_stdlib_emojiData,
                                                  _swift_stdlib_emojiData_ranks);

  // If we don't have an index into the data indices, then this scalar is not an
  // extended pictographic.
  if (dataIdx == INTPTR_MAX) {
    return false;
  }

  return true;
#endif
}
