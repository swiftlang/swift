//===--- InlineBitfield.h - Inline bitfield macros --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides macros to simplify inline/intrusive bitfield logic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_INLINE_BITFIELD_H
#define SWIFT_INLINE_BITFIELD_H

#include "llvm/Support/Compiler.h"
#include <cstdint>

#if defined(_MSC_VER)
#include <intrin.h>
#endif

// Boilerplate namespace in case we add non-macros.
namespace swift {

/// NOTE: When passing the bit count to these macros, please do NOT precompute
/// the total. Instead, sum the bit counts in field order. This makes visually
/// verifying that all the fields are accounted for easier. For example:
/// SWIFT_INLINE_BITFIELD(Foo, Bar, 1+3+7+2, w:1, x:3, y:7, z:2);

/// Define a base bitfield for type 'T' with 'C' bits used.
///
/// Please note that the 'base' type does not need to be the root class in a
/// hierarchy. If a superclass bitfield is full, then a subclass can start a new
/// bitfield union for its subclasses to use.
#define SWIFT_INLINE_BITFIELD_BASE(T, C, ...) \
  LLVM_PACKED_START \
  class T##Bitfield { \
    friend class T; \
    uint64_t __VA_ARGS__; \
    uint64_t : 64 - (C); /* Better code gen */ \
  } T; \
  LLVM_PACKED_END \
  enum { Num##T##Bits = (C) }; \
  static_assert(sizeof(T##Bitfield) <= 8, "Bitfield overflow")

/// Define an bitfield for type 'T' with parent class 'U' and 'C' bits used.
#define SWIFT_INLINE_BITFIELD_TEMPLATE(T, U, C, HC, ...) \
  LLVM_PACKED_START \
  class T##Bitfield { \
    friend class T; \
    uint64_t : Num##U##Bits, __VA_ARGS__; \
    uint64_t : 64 - (Num##U##Bits + (HC) + (C)); /* Better code gen */ \
  } T; \
  LLVM_PACKED_END \
  enum { Num##T##Bits = Num##U##Bits + (C) }; \
  static_assert(sizeof(T##Bitfield) <= 8, "Bitfield overflow")

#define SWIFT_INLINE_BITFIELD(T, U, C, ...) \
  SWIFT_INLINE_BITFIELD_TEMPLATE(T, U, C, 0, __VA_ARGS__)

/// Define a full bitfield for type 'T' that uses all of the remaining bits in
/// the inline bitfield.
///
/// For optimal code gen, place naturally sized fields at the end, with the
/// largest naturally sized field at the very end. For example:
///
/// SWIFT_INLINE_BITFIELD_FULL(Foo, Bar, 1+8+16,
///   flag : 1,
///   : NumPadBits, // pad the center, not the end
///   x : 8,
///   y : 16
/// );
#define SWIFT_INLINE_BITFIELD_FULL(T, U, C, ...) \
  LLVM_PACKED_START \
  class T##Bitfield { \
    friend class T; \
    enum { NumPadBits = 64 - (Num##U##Bits + (C)) }; \
    uint64_t : Num##U##Bits, __VA_ARGS__; \
  } T; \
  LLVM_PACKED_END \
  static_assert(sizeof(T##Bitfield) <= 8, "Bitfield overflow")

/// Define a full bitfield for type 'T' that uses all of the remaining bits in
/// the inline bitfield. We allow for 'T' to have a single generic parameter.
///
/// For optimal code gen, place naturally sized fields at the end, with the
/// largest naturally sized field at the very end. For example:
///
/// SWIFT_INLINE_BITFIELD_FULL(Foo, Bar, 1+8+16,
///   flag : 1,
///   : NumPadBits, // pad the center, not the end
///   x : 8,
///   y : 16
/// );
///
/// NOTE: All instances of Foo will access via the same bitfield entry even if
/// they differ in the templated value!
#define SWIFT_INLINE_BITFIELD_FULL_TEMPLATE(T, U, C, ...)                      \
  LLVM_PACKED_START                                                            \
  class T##Bitfield {                                                          \
    template <typename TTy>                                                    \
    friend class T;                                                            \
    enum { NumPadBits = 64 - (Num##U##Bits + (C)) };                           \
    uint64_t : Num##U##Bits, __VA_ARGS__;                                      \
  } T;                                                                         \
  LLVM_PACKED_END                                                              \
  static_assert(sizeof(T##Bitfield) <= 8, "Bitfield overflow")

/// Define an empty bitfield for type 'T'.
#define SWIFT_INLINE_BITFIELD_EMPTY(T, U) \
  enum { Num##T##Bits = Num##U##Bits }

// XXX/HACK: templated max() doesn't seem to work in a bitfield size context.
constexpr unsigned bitmax(unsigned a, unsigned b) {
  return a > b ? a : b;
}

constexpr unsigned countBitsUsed(uint64_t arg) {
// Assumes uint64_t is the same as unsigned long long
#if defined(_MSC_VER)
#if defined(_M_AMD64)
  return 64u - static_cast<unsigned>(__lzcnt64(arg));
#elseif  defined(_M_ARM) || defined(_M_ARM64)
  return 64u - static_cast<unsigned>(_CountLeadingZeros64(arg));
#elseif __has_builtin(__builtin_ctzll) || defined(__GNUC__)
  return 64u - __builtin_clzll(static_cast<unsigned long long>(arg));
#else
# error unsupported architecture
#endif
#else
  return 64u - __builtin_clzll(static_cast<unsigned long long>(arg));
#endif
}

} // end namespace swift

#endif // SWIFT_INLINE_BITFIELD_H
