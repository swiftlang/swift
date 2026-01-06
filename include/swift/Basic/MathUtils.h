//===--- GenericMetadataBuilder.h - Math utilities. -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utility functions for math operations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_MATH_UTILS_H
#define SWIFT_BASIC_MATH_UTILS_H

#include "Compiler.h"
#include <cstddef>
#include <cstdint>

#if SWIFT_COMPILER_IS_MSVC
#include <intrin.h>
#endif

namespace swift {

/// Round the given value up to the given alignment, as a power of two.
template <class T>
static inline constexpr T roundUpToAlignment(T offset, T alignment) {
  return (offset + alignment - 1) & ~(alignment - 1);
}

/// Round the given value up to the given alignment, expressed as a mask (a
/// power of two minus one).
static inline size_t roundUpToAlignMask(size_t size, size_t alignMask) {
  return (size + alignMask) & ~alignMask;
}

static inline unsigned popcount(unsigned value) {
#if SWIFT_COMPILER_IS_MSVC && (defined(_M_IX86) || defined(_M_X64))
  // The __popcnt intrinsic is only available when targetting x86{_64} with MSVC.
  return __popcnt(value);
#elif __has_builtin(__builtin_popcount)
  return __builtin_popcount(value);
#else
  // From llvm/ADT/bit.h which the runtime doesn't have access to (yet?)
  uint32_t v = value;
  v = v - ((v >> 1) & 0x55555555);
  v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
  return int(((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24);
#endif
}

} // namespace swift

#endif // #ifndef SWIFT_BASIC_MATH_UTILS_H
