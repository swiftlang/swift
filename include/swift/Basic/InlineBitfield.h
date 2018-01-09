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

/// Define an empty bitfield for type 'T'.
#define SWIFT_INLINE_BITFIELD_EMPTY(T, U) \
  enum { Num##T##Bits = Num##U##Bits }

// XXX/HACK: templated max() doesn't seem to work in a bitfield size context.
constexpr unsigned bitmax(unsigned a, unsigned b) {
  return a > b ? a : b;
}

constexpr unsigned countBitsUsed(uint64_t arg) {
  // Is there a C++ "std::countLeadingZeros()"?
  return (arg & 1ull << 63 ? 63 :
          arg & 1ull << 62 ? 62 :
          arg & 1ull << 61 ? 61 :
          arg & 1ull << 60 ? 60 :
          arg & 1ull << 59 ? 59 :
          arg & 1ull << 58 ? 58 :
          arg & 1ull << 57 ? 57 :
          arg & 1ull << 56 ? 56 :
          arg & 1ull << 55 ? 55 :
          arg & 1ull << 54 ? 54 :
          arg & 1ull << 53 ? 53 :
          arg & 1ull << 52 ? 52 :
          arg & 1ull << 51 ? 51 :
          arg & 1ull << 50 ? 50 :
          arg & 1ull << 49 ? 49 :
          arg & 1ull << 48 ? 48 :
          arg & 1ull << 47 ? 47 :
          arg & 1ull << 46 ? 46 :
          arg & 1ull << 45 ? 45 :
          arg & 1ull << 44 ? 44 :
          arg & 1ull << 43 ? 43 :
          arg & 1ull << 42 ? 42 :
          arg & 1ull << 41 ? 41 :
          arg & 1ull << 40 ? 40 :
          arg & 1ull << 39 ? 39 :
          arg & 1ull << 38 ? 38 :
          arg & 1ull << 37 ? 37 :
          arg & 1ull << 36 ? 36 :
          arg & 1ull << 35 ? 35 :
          arg & 1ull << 34 ? 34 :
          arg & 1ull << 33 ? 33 :
          arg & 1ull << 32 ? 32 :
          arg & 1ull << 31 ? 31 :
          arg & 1ull << 30 ? 30 :
          arg & 1ull << 29 ? 29 :
          arg & 1ull << 28 ? 28 :
          arg & 1ull << 27 ? 27 :
          arg & 1ull << 26 ? 26 :
          arg & 1ull << 25 ? 25 :
          arg & 1ull << 24 ? 24 :
          arg & 1ull << 23 ? 23 :
          arg & 1ull << 22 ? 22 :
          arg & 1ull << 21 ? 21 :
          arg & 1ull << 20 ? 20 :
          arg & 1ull << 19 ? 19 :
          arg & 1ull << 18 ? 18 :
          arg & 1ull << 17 ? 17 :
          arg & 1ull << 16 ? 16 :
          arg & 1ull << 15 ? 15 :
          arg & 1ull << 14 ? 14 :
          arg & 1ull << 13 ? 13 :
          arg & 1ull << 12 ? 12 :
          arg & 1ull << 11 ? 11 :
          arg & 1ull << 10 ? 10 :
          arg & 1ull << 9 ? 9 :
          arg & 1ull << 8 ? 8 :
          arg & 1ull << 7 ? 7 :
          arg & 1ull << 6 ? 6 :
          arg & 1ull << 5 ? 5 :
          arg & 1ull << 4 ? 4 :
          arg & 1ull << 3 ? 3 :
          arg & 1ull << 2 ? 2 :
          arg & 1ull << 1 ? 1 : 0
      ) + 1;
}

} // end namespace swift

#endif // SWIFT_INLINE_BITFIELD_H
