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

#include <cstddef>

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
#if SWIFT_COMPILER_IS_MSVC
  return __popcnt(value);
#else
  // Assume we have a compiler with this intrinsic.
  return __builtin_popcount(value);
#endif
}

} // namespace swift

#endif // #ifndef SWIFT_BASIC_MATH_UTILS_H
