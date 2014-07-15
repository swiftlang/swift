//===--- Data.h - Side Car Data Types ---------------------------*- C++ -*-===//
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
// This file defines data types used in the representation of side-car data.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIDE_TYPES_H
#define SWIFT_SIDE_TYPES_H
#include "swift/Basic/Optional.h"
#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <climits>

namespace swift {
namespace side_car {

using llvm::StringRef;

/// Describes the nullability of a particular value, whether it is a property,
/// parameter type, or result type.
enum class NullableKind : unsigned {
  /// The parameter is non-nullable.
  NonNullable = 0,
  /// The parameter is nullable.
  Nullable,
  /// The nullability of the parameter is unknown.
  Unknown
};

/// Describes whether to classify a factory method as an initializer.
enum class FactoryAsInitKind {
  /// Infer based on name and type (the default).
  Infer,
  /// Treat as a class method.
  AsClassMethod,
  /// Treat as an initializer.
  AsInitializer
};

/// Describes side-car data for an Objective-C class.
class ObjCClassInfo {
  /// Whether this class has a default nullability.
  unsigned HasDefaultNullability : 1;

  /// The default nullability.
  unsigned DefaultNullability : 2;

public:
  ObjCClassInfo()
    : HasDefaultNullability(0),
      DefaultNullability(0)
  { }

  /// Determine the default nullability for properties and methods of this
  /// class.
  ///
  /// \returns the default nullability, if implied, or Nothing if there is no
  Optional<NullableKind> getDefaultNullability() const {
    if (HasDefaultNullability)
      return static_cast<NullableKind>(DefaultNullability);

    return Nothing;
  }

  /// Set the default nullability for properties and methods of this class.
  void setDefaultNullability(NullableKind kind) {
    HasDefaultNullability = true;
    DefaultNullability = static_cast<unsigned>(kind);
  }
};

/// Describes side-car data for an Objective-C property.
class ObjCPropertyInfo {
  /// Whether this property has been audited for nullability.
  unsigned NullabilityAudited : 1;

  /// The kind of nullability for this property. Only valid if the nullability
  /// has been audited.
  unsigned Nullable : 2;

public:
  ObjCPropertyInfo()
    : NullabilityAudited(false),
      Nullable(0) { }

  Optional<NullableKind> getNullability() const {
    if (NullabilityAudited)
      return static_cast<NullableKind>(Nullable);

    return Nothing;
  }

  void setNullabilityAudited(NullableKind kind) {
    NullabilityAudited = true;
    Nullable = static_cast<unsigned>(kind);
  }
};

/// Describes side-car data for an Objective-C method.
class ObjCMethodInfo {
private:
  static unsigned const NullableKindMask = 0x3;
  static unsigned const NullableKindSize = 2;

public:
  /// Whether this is a designated initializer of its class.
  unsigned DesignatedInit : 1;

  /// Whether to treat this method as a factory or initializer.
  unsigned FactoryAsInit : 2;

  /// Whether this method is marked unavailable.
  unsigned Unavailable : 1;

  /// Whether the signature has been audited with respect to nullability.
  /// If yes, we consider all types to be non-nullable unless otherwise noted.
  /// If this flag is not set, the pointer types are considered to have
  /// unknown nullability.
  unsigned NullabilityAudited : 1;

  /// Number of types whose nullability is encoded with the NullabilityPayload.
  unsigned NumAdjustedNullable : 8;

  /// Stores the nullability of the return type and the parameters.
  //  NullableKindSize bits are used to encode the nullability. The info
  //  about the return type is stored at position 0, followed by the nullability
  //  of the parameters.
  uint64_t NullabilityPayload = 0;

  /// Message to use when this method is unavailable.
  StringRef UnavailableMsg;

  ObjCMethodInfo()
    : DesignatedInit(false),
      FactoryAsInit(static_cast<unsigned>(FactoryAsInitKind::Infer)),
      Unavailable(false),
      NullabilityAudited(false),
      NumAdjustedNullable(0) { }

  FactoryAsInitKind getFactoryAsInitKind() const {
    return static_cast<FactoryAsInitKind>(FactoryAsInit);
  }

  void setFactoryAsInitKind(FactoryAsInitKind kind) {
    FactoryAsInit = static_cast<unsigned>(kind);
  }

  void addTypeInfo(unsigned index, NullableKind kind) {
    assert(index <=
           (sizeof(NullabilityPayload) * CHAR_BIT)/NullableKindSize);
    assert(static_cast<unsigned>(kind) < NullableKindMask);
    assert(NullabilityAudited);
    unsigned kindValue =
    (static_cast<unsigned>(kind)) << (index * NullableKindSize);
    NullabilityPayload |= kindValue;
  }

private:
  NullableKind getTypeInfo(unsigned index) {
    assert(NullabilityAudited &&
           "Checking the type adjustment on non-audited method.");
    // If we don't have info about this parameter, return the default.
    if (index > NumAdjustedNullable)
      return NullableKind::NonNullable;
    return static_cast<NullableKind>(( NullabilityPayload
                                          >> (index * NullableKindSize) )
                                         & NullableKindMask);
  }

public:
  NullableKind getParamTypeInfo(unsigned index) {
    return getTypeInfo(index + 1);
  }
  
  NullableKind getReturnTypeInfo() {
    return getTypeInfo(0);
  }
};

} // end namespace side_car
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_TYPES_H
