//===--- RecursiveTypeProperties.h ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RECURSIVE_TYPE_PROPERTIES_H
#define SWIFT_RECURSIVE_TYPE_PROPERTIES_H

#include "swift/Basic/InlineBitfield.h"

namespace swift {

/// Various properties of types that are primarily defined recursively
/// on structural types.
class RecursiveTypeProperties {
public:
  /// A single property.
  ///
  /// Note that the property polarities should be chosen so that 0 is
  /// the correct default value and bitwise-or correctly merges things.
  enum Property : unsigned {
    /// This type expression contains a TypeVariableType.
    HasTypeVariable      = 0x01,

    /// This type expression contains a PrimaryArchetypeType
    /// or PackArchetypeType.
    HasPrimaryArchetype  = 0x02,

    /// This type expression contains a GenericTypeParamType.
    HasTypeParameter     = 0x04,

    /// Whether this type expression contains an unbound generic type.
    HasUnboundGeneric    = 0x08,

    /// This type expression contains an LValueType other than as a
    /// function input, and can be loaded to convert to an rvalue.
    IsLValue             = 0x10,

    /// This type expression contains an opened existential ArchetypeType.
    HasOpenedExistential = 0x20,

    /// This type expression contains a DynamicSelf type.
    HasDynamicSelf       = 0x40,

    /// This type contains an Error type.
    HasError             = 0x80,

    /// This type contains an Error type without an underlying original type.
    HasBareError         = 0x100,

    /// Whether this type contains a JoinType or MeetType.
    HasJoinOrMeet        = 0x200,

    /// This type contains an OpaqueTypeArchetype.
    HasOpaqueArchetype   = 0x400,

    /// This type contains a type placeholder.
    HasPlaceholder       = 0x800,

    /// This type contains a generic type parameter that is declared as a
    /// parameter pack.
    HasParameterPack = 0x1000,

    /// This type contains a parameterized existential type \c any P<T>.
    HasParameterizedExistential = 0x2000,

    /// This type contains an ElementArchetypeType.
    HasElementArchetype = 0x4000,

    /// Whether the type is allocated in the constraint solver arena. This can
    /// differ from \c HasTypeVariable for types such as holes, which do
    /// not have type variables, but we still want to allocate in the solver if
    /// they have a type variable originator.
    SolverAllocated = 0x8000,

    /// Contains a PackType.
    HasPack = 0x10000,

    /// Contains a PackArchetypeType. Also implies HasPrimaryArchetype.
    HasPackArchetype = 0x20000,

    /// Whether this type contains an unsafe type.
    IsUnsafe = 0x040000,

    /// This type contains a type hole.
    HasHole = 0x080000,

    Last_Property = HasHole
  };
  enum { BitWidth = countBitsUsed(Property::Last_Property) };

private:
  unsigned Bits;

public:
  RecursiveTypeProperties() : Bits(0) {}
  RecursiveTypeProperties(Property prop) : Bits(prop) {}
  explicit RecursiveTypeProperties(unsigned bits) : Bits(bits) {}

  /// Return these properties as a bitfield.
  unsigned getBits() const { return Bits; }

  /// Does a type with these properties structurally contain a type
  /// variable?
  bool hasTypeVariable() const { return Bits & HasTypeVariable; }

  /// Does a type with these properties structurally contain a primary
  /// or pack archetype? These are the archetypes instantiated from a
  /// primary generic environment.
  bool hasPrimaryArchetype() const { return Bits & HasPrimaryArchetype; }

  /// Does a type with these properties structurally contain an
  /// archetype from an opaque type declaration?
  bool hasOpaqueArchetype() const { return Bits & HasOpaqueArchetype; }

  /// Does a type with these properties have a type parameter somewhere in it?
  bool hasTypeParameter() const { return Bits & HasTypeParameter; }

  /// Is a type with these properties an lvalue?
  bool isLValue() const { return Bits & IsLValue; }

  /// Does this type contain an error?
  bool hasError() const { return Bits & HasError; }

  /// Does this type contain an error without an original type?
  bool hasBareError() const { return Bits & HasBareError; }

  /// Does a type with these properties structurally contain an
  /// opened existential archetype?
  bool hasOpenedExistential() const { return Bits & HasOpenedExistential; }

  /// Does a type with these properties structurally contain an
  /// opened element archetype?
  bool hasElementArchetype() const { return Bits & HasElementArchetype; }

  /// Whether the type is allocated in the constraint solver arena. This can
  /// differ from \c hasTypeVariable() for types such as holes, which
  /// do not have type variables, but we still want to allocate in the solver if
  /// they have a type variable originator.
  bool isSolverAllocated() const { return Bits & SolverAllocated; }

  /// Determine whether the type involves a primary, pack or local archetype.
  bool hasArchetype() const {
    return hasPrimaryArchetype() || hasLocalArchetype();
  }

  /// Does a type with these properties structurally contain a local
  /// archetype?
  bool hasLocalArchetype() const {
    return hasOpenedExistential() || hasElementArchetype();
  }

  /// Does a type with these properties structurally contain a
  /// reference to DynamicSelf?
  bool hasDynamicSelf() const { return Bits & HasDynamicSelf; }

  /// Does a type with these properties structurally contain an unbound
  /// generic type?
  bool hasUnboundGeneric() const { return Bits & HasUnboundGeneric; }

  /// Does a type with these properties structurally contain a placeholder?
  bool hasPlaceholder() const { return Bits & HasPlaceholder; }

  /// Does a type with these properties structurally contain a hole?
  bool hasHole() const { return Bits & HasHole; }

  bool hasParameterPack() const { return Bits & HasParameterPack; }

  bool hasPack() const { return Bits & HasPack; }

  bool hasPackArchetype() const { return Bits & HasPackArchetype; }

  bool isUnsafe() const { return Bits & IsUnsafe; }

  bool hasJoinOrMeet() const { return Bits & HasJoinOrMeet; }

  /// Does a type with these properties structurally contain a
  /// parameterized existential type?
  bool hasParameterizedExistential() const {
    return Bits & HasParameterizedExistential;
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
