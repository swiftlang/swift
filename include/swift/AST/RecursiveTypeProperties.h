//===--- RecursiveTypeProperties.h ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines RecursiveTypeProperties, a helper structure for
// properties of types that are generally recursively propagated by
// component types.
//
// Note that these properties generally refer to the type *expression*,
// not the *storage* of the type. That is, the component types here are
// things like the type arguments of a generic type or the parameter types
// of a function type, not the types of stored properties.
//
// The polarities of properties should be chosen so that 0 is the correct
// default value and a bitwise-or will correctly merge properties from
// different component types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RECURSIVE_TYPE_PROPERTIES_H
#define SWIFT_RECURSIVE_TYPE_PROPERTIES_H

#include "swift/Basic/InlineBitfield.h"

namespace swift {

/// Various properties of types that are primarily defined recursively
/// on structural types.
class RecursiveTypeProperties {
  // Generate the bit-offsets of the properties.
  enum class PropertyIndex : unsigned {
#define PROPERTY(ID, METHOD_NAME) \
    ID,
#include "RecursiveTypeProperties.def"
    NumProperties
  };

public:
  enum Property : unsigned {
#define PROPERTY(ID, METHOD_NAME) \
    ID = 1U << unsigned(PropertyIndex::ID),
#include "RecursiveTypeProperties.def"
  };

  static constexpr unsigned BitWidth = unsigned(PropertyIndex::NumProperties);

private:
  unsigned Bits;

public:
  RecursiveTypeProperties() : Bits(0) {}
  RecursiveTypeProperties(Property prop) : Bits(prop) {}
  explicit RecursiveTypeProperties(unsigned bits) : Bits(bits) {}

  /// Return these properties as a bitfield.
  unsigned getBits() const { return Bits; }

#define PROPERTY(ID, METHOD_NAME) \
  bool METHOD_NAME() const { return Bits & ID; }
#include "RecursiveTypeProperties.def"

  /// Determine whether the type involves a primary, pack or local archetype.
  bool hasArchetype() const {
    return hasPrimaryArchetype() || hasLocalArchetype();
  }

  /// Does a type with these properties structurally contain a local
  /// archetype?
  bool hasLocalArchetype() const {
    return hasOpenedExistential() || hasElementArchetype();
  }

  /// Returns the set of properties present in either set.
  friend RecursiveTypeProperties operator|(Property lhs, Property rhs) {
    return RecursiveTypeProperties(unsigned(lhs) | unsigned(rhs));
  }
  friend RecursiveTypeProperties operator|(RecursiveTypeProperties lhs,
                                           RecursiveTypeProperties rhs) {
    return RecursiveTypeProperties(lhs.Bits | rhs.Bits);
  }

  /// Add any properties in the right-hand set to this set.
  RecursiveTypeProperties &operator|=(RecursiveTypeProperties other) {
    Bits |= other.Bits;
    return *this;
  }
  /// Restrict this to only the properties in the right-hand set.
  RecursiveTypeProperties &operator&=(RecursiveTypeProperties other) {
    Bits &= other.Bits;
    return *this;
  }

  /// Remove the HasTypeParameter property from this set.
  void removeHasTypeParameter() {
    Bits &= ~HasTypeParameter;
  }

  /// Remove the IsUnsafe property from this set.
  void removeIsUnsafe() {
    Bits &= ~IsUnsafe;
  }

  /// Test for a particular property in this set.
  bool operator&(Property prop) const {
    return Bits & prop;
  }
};

inline RecursiveTypeProperties operator~(RecursiveTypeProperties::Property P) {
  return RecursiveTypeProperties(~unsigned(P));
}

} // end namespace swift

#endif
