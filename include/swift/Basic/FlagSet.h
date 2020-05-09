//===--- FlagSet.h - Helper class for opaque flag types ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the FlagSet template, a class which makes it easier to
// define opaque flag types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_FLAGSET_H
#define SWIFT_BASIC_FLAGSET_H

#include <type_traits>
#include <assert.h>

namespace swift {

/// A template designed to simplify the task of defining a wrapper type
/// for a flags bitfield.
///
/// Unfortunately, this doesn't currently support functional-style
/// building patterns, which means this can't practically be used for
/// types that need to be used in constant expressions.
template <typename IntType>
class FlagSet {
  static_assert(std::is_integral<IntType>::value,
                "storage type for FlagSet must be an integral type");
  IntType Bits;

protected:
  template <unsigned BitWidth>
  static constexpr IntType lowMaskFor() {
    return IntType((1 << BitWidth) - 1);
  }

  template <unsigned FirstBit, unsigned BitWidth = 1>
  static constexpr IntType maskFor() {
    return lowMaskFor<BitWidth>() << FirstBit;
  }

  constexpr FlagSet(IntType bits = 0) : Bits(bits) {}

  /// Read a single-bit flag.
  template <unsigned Bit>
  bool getFlag() const {
    return Bits & maskFor<Bit>();
  }

  /// Set a single-bit flag.
  template <unsigned Bit>
  void setFlag(bool value) {
    if (value) {
      Bits |= maskFor<Bit>();
    } else {
      Bits &= ~maskFor<Bit>();
    }
  }

  /// Read a multi-bit field.
  template <unsigned FirstBit, unsigned BitWidth, typename FieldType = IntType>
  FieldType getField() const {
    return FieldType((Bits >> FirstBit) & lowMaskFor<BitWidth>());
  }

  /// Assign to a multi-bit field.
  template <unsigned FirstBit, unsigned BitWidth, typename FieldType = IntType>
  void setField(typename std::enable_if<true, FieldType>::type value) {
    // Note that we suppress template argument deduction for FieldType.
    assert(IntType(value) <= lowMaskFor<BitWidth>() && "value out of range");
    Bits = (Bits & ~maskFor<FirstBit, BitWidth>())
         | (IntType(value) << FirstBit);
  }

  // A convenient macro for defining a getter and setter for a flag.
  // Intended to be used in the body of a subclass of FlagSet.
#define FLAGSET_DEFINE_FLAG_ACCESSORS(BIT, GETTER, SETTER) \
  bool GETTER() const {                                    \
    return this->template getFlag<BIT>();                  \
  }                                                        \
  void SETTER(bool value) {                                \
    this->template setFlag<BIT>(value);                    \
  }

  // A convenient macro for defining a getter and setter for a field.
  // Intended to be used in the body of a subclass of FlagSet.
#define FLAGSET_DEFINE_FIELD_ACCESSORS(BIT, WIDTH, TYPE, GETTER, SETTER) \
  TYPE GETTER() const {                                                  \
    return this->template getField<BIT, WIDTH, TYPE>();                  \
  }                                                                      \
  void SETTER(TYPE value) {                                              \
    this->template setField<BIT, WIDTH, TYPE>(value);                    \
  }

  // A convenient macro to expose equality operators.
  // These can't be provided directly by FlagSet because that would allow
  // different flag sets to be compared if they happen to have the same
  // underlying type.
#define FLAGSET_DEFINE_EQUALITY(TYPENAME)                                \
  friend bool operator==(TYPENAME lhs, TYPENAME rhs) {                   \
    return lhs.getOpaqueValue() == rhs.getOpaqueValue();                 \
  }                                                                      \
  friend bool operator!=(TYPENAME lhs, TYPENAME rhs) {                   \
    return lhs.getOpaqueValue() != rhs.getOpaqueValue();                 \
  }

public:
  /// Get the bits as an opaque integer value.
  IntType getOpaqueValue() const {
    return Bits;
  }
};

} // end namespace swift

#endif
