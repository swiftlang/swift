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

#ifndef SWIFT_STDLIB_SHIMS_UNICODEDATA_H
#define SWIFT_STDLIB_SHIMS_UNICODEDATA_H

#include "SwiftStdint.h"
#include "Visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint16_t _swift_stdlib_getNormData(__swift_uint32_t scalar);

SWIFT_RUNTIME_STDLIB_INTERNAL
const __swift_uint8_t * const _swift_stdlib_nfd_decompositions;

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _swift_stdlib_getDecompositionEntry(__swift_uint32_t scalar);

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _swift_stdlib_getComposition(__swift_uint32_t x,
                                              __swift_uint32_t y);

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_intptr_t _swift_stdlib_getMphIdx(__swift_uint32_t scalar,
                                         __swift_intptr_t levels,
                                         const __swift_uint64_t * const *keys,
                                         const __swift_uint16_t * const *ranks,
                                         const __swift_uint16_t * const sizes);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_STDLIB_SHIMS_UNICODEDATA_H
