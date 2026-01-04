//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_UNICODEDATA_H
#define SWIFT_STDLIB_UNICODEDATA_H

#include "swift/shims/SwiftStdbool.h"
#include "swift/shims/SwiftStdint.h"
#include "swift/shims/Visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_intptr_t _swift_stdlib_getMphIdx(__swift_uint32_t scalar,
                                         __swift_intptr_t levels,
                                         const __swift_uint64_t * const *keys,
                                         const __swift_uint16_t * const *ranks,
                                         const __swift_uint16_t * const sizes);

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_intptr_t _swift_stdlib_getScalarBitArrayIdx(__swift_uint32_t scalar,
                                              const __swift_uint64_t *bitArrays,
                                              const __swift_uint16_t *ranks);

#ifdef __cplusplus
} // extern "C"
#endif

#ifdef __cplusplus
namespace swift {
  /// Redeclared from the runtime headers, which we do not want to include here.
  SWIFT_NORETURN
  void swift_abortDisabledUnicodeSupport();
}
#endif

#endif // SWIFT_STDLIB_SHIMS_UNICODEDATA_H   
