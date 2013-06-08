//===- Fixnum.h - An integer type with an explicit bit width ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file Declares Fixnum, an integer type with an explicit bit width,
/// and utilities for working with bit widths of integers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_FIXNUM_H
#define SWIFT_BASIC_FIXNUM_H

#include "llvm/Support/MathExtras.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <limits>

namespace swift {
  /// Defines a member #type that is the smallest signed integer that can hold
  /// a value with the given bit width.
  template <unsigned Bits>
  struct int_least {
    static_assert(Bits <= 64, "too many bits");
    using type = typename std::conditional<(Bits <= 8), int_least8_t,
                 typename std::conditional<(Bits <= 16), int_least16_t,
                 typename std::conditional<(Bits <= 32), int_least32_t,
                 int_least64_t>::type>::type>::type;
  };

  /// Defines a member #type that is the smallest unsigned integer that can hold
  /// a value with the given bit width.
  template <unsigned Bits>
  struct uint_least {
    using type =
      typename std::make_unsigned<typename int_least<Bits>::type>::type;
  };

  /// A wrapper for an integer type that is guaranteed to only use a certain
  /// number of bits.
  ///
  /// This can be used to treat an integer like a pointer with low bits free.
  ///
  /// Note that if the integer type is signed, \p Bits must include the sign
  /// bit, just like a bitfield.
  template <unsigned Bits, typename IntType = typename uint_least<Bits>::type>
  class Fixnum {
    static_assert(Bits <= (std::numeric_limits<IntType>::digits +
                           std::numeric_limits<IntType>::is_signed),
                  "too many bits for integer type");

    IntType Value;

    void assertValid() const {
      assert((std::is_signed<IntType>::value ? llvm::isInt<Bits>(Value)
                                             : llvm::isUInt<Bits>(Value)) &&
             "value exceeds limited bit width");      
    }

  public:
    using value_type = IntType;

    Fixnum() : Value(0) {}

    /*implicit*/ Fixnum(IntType val) : Value(val) {
      assertValid();
    }

    /// Initialize a Fixnum from another, smaller Fixnum.
    ///
    /// This is always safe and thus permitted as an implicit coercion.
    template <unsigned OtherBits, typename OtherIntType>
    /*implicit*/ Fixnum(
         const typename std::enable_if<(OtherBits < Bits),
                                       Fixnum<OtherBits,
                                              OtherIntType>>::type &other)
       : Value(static_cast<IntType>(other)) {}

    /// Initialize a Fixnum from another of a different width.
    ///
    /// This is permitted, but checked with assertions. It must be explicitly
    /// requested -- it is not a valid implicit conversion.
    template <unsigned OtherBits, typename OtherIntType>
    explicit Fixnum(const Fixnum<OtherBits, OtherIntType> &other) {
      operator=(other);
    }

    /// Assign to a Fixnum from another of a different width.
    ///
    /// This is permitted, but checked with assertions.
    template <unsigned OtherBits, typename OtherIntType>
    Fixnum &operator=(const Fixnum<OtherBits, OtherIntType> &other) {
      Value = static_cast<IntType>(other);
      assert(static_cast<OtherIntType>(Value) == other &&
             "cannot represent the same value");
      assert(((Value < 0) == (other < 0)) && "signedness mismatch");
      assertValid();
      return *this;
    }

    /*implicit*/ operator IntType() const {
      return Value;
    }

    Fixnum &operator++() {
      assert((Value != std::numeric_limits<IntType>::max()) &&
             "increment would cause wraparound");
      ++Value;
      assertValid();
      return *this;
    }

    Fixnum operator++(int) {
      assert((Value != std::numeric_limits<IntType>::max()) &&
             "increment would cause wraparound");
      Fixnum result = *this;
      ++Value;
      assertValid();
      return result;
    }

    Fixnum &operator--() {
      assert((Value != std::numeric_limits<IntType>::min()) &&
             "decrement would cause wraparound");
      --Value;
      assertValid();
      return *this;
    }

    Fixnum operator--(int) {
      assert((Value != std::numeric_limits<IntType>::min()) &&
             "decrement would cause wraparound");
      Fixnum result = *this;
      --Value;
      assertValid();
      return result;
    }
  };
} // end namespace swift

// Fixnum can be treated like a pointer with low bits free if it is no
// larger than a pointer.
template<unsigned IntBits, typename IntType>
class llvm::PointerLikeTypeTraits<swift::Fixnum<IntBits, IntType>> {
  using IntPointerType =
    typename std::conditional<std::is_signed<IntType>::value,
                              intptr_t, uintptr_t>::type;

public:
  static_assert(sizeof(IntType) <= sizeof(IntPointerType),
                "Fixnum is too big to fit in a pointer");

  static inline void *
  getAsVoidPointer(const swift::Fixnum<IntBits, IntType> &I) {
    auto opaqueValue = static_cast<IntPointerType>(I) << NumLowBitsAvailable;
    return reinterpret_cast<void *>(opaqueValue);
  }
  
  static inline swift::Fixnum<IntBits, IntType>
  getFromVoidPointer(const void *P) {
    auto opaqueValue = reinterpret_cast<IntPointerType>(P);
    return static_cast<IntType>(opaqueValue >> NumLowBitsAvailable);
  }

  enum {
    NumLowBitsAvailable = std::numeric_limits<uintptr_t>::digits - IntBits
  };
};

#endif
