//===--- Types.h - API Notes Data Types --------------------------*- C++ -*-===//
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
// This file defines data types used in the representation of API notes data.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIDE_TYPES_H
#define SWIFT_SIDE_TYPES_H
#include "swift/Basic/Optional.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <climits>
#include <initializer_list>

namespace swift {
namespace api_notes {

/// The file extension used for the binary representation of API notes.
static const char BINARY_APINOTES_EXTENSION[] = "apinotesc";

using llvm::StringRef;

/// Describes the nullability of a particular value, whether it is a property,
/// parameter type, or result type.
enum class NullableKind : unsigned {
  /// The parameter is non-nullable.
  NonNullable = 0,
  /// The parameter is nullable.
  Nullable,
  /// The nullability of the parameter is unknown.
  Unknown,
  /// The nullability of the parameter has not been entered.
  Absent
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

/// Opaque context ID used to refer to an Objective-C class or protocol.
class ContextID {
public:
  unsigned Value;

  explicit ContextID(unsigned value) : Value(value) { }
};

/// Describes API notes data for any entity.
///
/// This is used as the base of
class CommonEntityInfo {
public:
  /// Message to use when this entity is unavailable.
  std::string UnavailableMsg;

  /// Whether this entity is marked unavailable.
  unsigned Unavailable : 1;

  CommonEntityInfo() : Unavailable(0) { }

  friend bool operator==(const CommonEntityInfo &lhs,
                         const CommonEntityInfo &rhs) {
    return lhs.UnavailableMsg == rhs.UnavailableMsg &&
           lhs.Unavailable == rhs.Unavailable;
  }

  friend bool operator!=(const CommonEntityInfo &lhs,
                         const CommonEntityInfo &rhs) {
    return !(lhs == rhs);
  }
};

/// Describes API notes data for an Objective-C class or protocol.
class ObjCContextInfo : public CommonEntityInfo {
  /// Whether this class has a default nullability.
  unsigned HasDefaultNullability : 1;

  /// The default nullability.
  unsigned DefaultNullability : 2;

  /// Whether this class has designated initializers recorded.
  unsigned HasDesignatedInits : 1;

public:
  ObjCContextInfo()
    : CommonEntityInfo(),
      HasDefaultNullability(0),
      DefaultNullability(0),
      HasDesignatedInits(0)
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

  bool hasDesignatedInits() const { return HasDesignatedInits; }
  void setHasDesignatedInits(bool value) { HasDesignatedInits = value; }

  /// Strip off any information within the class information structure that is
  /// module-local, such as 'audited' flags.
  void stripModuleLocalInfo() {
    HasDefaultNullability = false;
    DefaultNullability = 0;
  }

  friend bool operator==(const ObjCContextInfo &lhs, const ObjCContextInfo &rhs) {
    return static_cast<const CommonEntityInfo &>(lhs) == rhs &&
           lhs.HasDefaultNullability == rhs.HasDefaultNullability &&
           lhs.DefaultNullability == rhs.DefaultNullability &&
           lhs.HasDesignatedInits == rhs.HasDesignatedInits;
  }

  friend bool operator!=(const ObjCContextInfo &lhs, const ObjCContextInfo &rhs) {
    return !(lhs == rhs);
  }

  friend ObjCContextInfo &operator|=(ObjCContextInfo &lhs,
                                   const ObjCContextInfo &rhs) {
    // Merge nullability.
    if (!lhs.getDefaultNullability()) {
      if (auto nullable = rhs.getDefaultNullability()) {
        lhs.setDefaultNullability(*nullable);
      }
    }

    lhs.HasDesignatedInits |= rhs.HasDesignatedInits;
    return lhs;
  }
  
  void dump(llvm::raw_ostream &os);
};

/// API notes for a variable/property.
class VariableInfo : public CommonEntityInfo {
  /// Whether this property has been audited for nullability.
  unsigned NullabilityAudited : 1;

  /// The kind of nullability for this property. Only valid if the nullability
  /// has been audited.
  unsigned Nullable : 2;

public:
  VariableInfo()
    : CommonEntityInfo(),
      NullabilityAudited(false),
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


  friend bool operator==(const VariableInfo &lhs, const VariableInfo &rhs) {
    return static_cast<const CommonEntityInfo &>(lhs) == rhs &&
           lhs.NullabilityAudited == rhs.NullabilityAudited &&
           lhs.Nullable == rhs.Nullable;
  }

  friend bool operator!=(const VariableInfo &lhs, const VariableInfo &rhs) {
    return !(lhs == rhs);
  }
};

/// Describes API notes data for an Objective-C property.
class ObjCPropertyInfo : public VariableInfo {
public:
  ObjCPropertyInfo() : VariableInfo() { }

  /// Merge class-wide information into the given property.
  friend ObjCPropertyInfo &operator|=(ObjCPropertyInfo &lhs,
                                      const ObjCContextInfo &rhs) {
    // Merge nullability.
    if (!lhs.getNullability()) {
      if (auto nullable = rhs.getDefaultNullability()) {
        lhs.setNullabilityAudited(*nullable);
      }
    }

    return lhs;
  }
};

/// A temporary reference to an Objective-C selector, suitable for
/// referencing selector data on the stack.
///
/// Instances of this struct do not store references to any of the
/// data they contain; it is up to the user to ensure that the data
/// referenced by the identifier list persists.
struct ObjCSelectorRef {
  unsigned NumPieces;
  llvm::ArrayRef<StringRef> Identifiers;
};

/// API notes for a function or method.
class FunctionInfo : public CommonEntityInfo {
private:
  static unsigned const NullableKindMask = 0x3;
  static unsigned const NullableKindSize = 2;

public:
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

  FunctionInfo()
    : CommonEntityInfo(),
      NullabilityAudited(false),
      NumAdjustedNullable(0) { }

  static unsigned getMaxNullabilityIndex() {
    return ((sizeof(NullabilityPayload) * CHAR_BIT)/NullableKindSize);
  }

  void addTypeInfo(unsigned index, NullableKind kind) {
    assert(index <= getMaxNullabilityIndex());
    assert(static_cast<unsigned>(kind) < NullableKindMask);
    NullabilityAudited = true;
    unsigned kindValue =
    (static_cast<unsigned>(kind)) << (index * NullableKindSize);
    NullabilityPayload |= kindValue;
  }

private:
  NullableKind getTypeInfo(unsigned index) const {
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
  NullableKind getParamTypeInfo(unsigned index) const {
    return getTypeInfo(index + 1);
  }
  
  NullableKind getReturnTypeInfo() const {
    return getTypeInfo(0);
  }

  friend bool operator==(const FunctionInfo &lhs, const FunctionInfo &rhs) {
    return static_cast<const CommonEntityInfo &>(lhs) == rhs &&
           lhs.NullabilityAudited == rhs.NullabilityAudited &&
           lhs.NumAdjustedNullable == rhs.NumAdjustedNullable &&
           lhs.NullabilityPayload == rhs.NullabilityPayload;
  }

  friend bool operator!=(const FunctionInfo &lhs, const FunctionInfo &rhs) {
    return !(lhs == rhs);
  }

};

/// Describes API notes data for an Objective-C method.
class ObjCMethodInfo : public FunctionInfo {
public:
  /// Whether this is a designated initializer of its class.
  unsigned DesignatedInit : 1;

  /// Whether to treat this method as a factory or initializer.
  unsigned FactoryAsInit : 2;

  /// Whether this is a required initializer.
  unsigned Required : 1;

  ObjCMethodInfo()
    : FunctionInfo(),
      DesignatedInit(false),
      FactoryAsInit(static_cast<unsigned>(FactoryAsInitKind::Infer)),
      Required(false) { }

  FactoryAsInitKind getFactoryAsInitKind() const {
    return static_cast<FactoryAsInitKind>(FactoryAsInit);
  }

  void setFactoryAsInitKind(FactoryAsInitKind kind) {
    FactoryAsInit = static_cast<unsigned>(kind);
  }

  friend bool operator==(const ObjCMethodInfo &lhs, const ObjCMethodInfo &rhs) {
    return static_cast<const FunctionInfo &>(lhs) == rhs &&
           lhs.DesignatedInit == rhs.DesignatedInit &&
           lhs.FactoryAsInit == rhs.FactoryAsInit &&
           lhs.Required == rhs.Required;
  }

  friend bool operator!=(const ObjCMethodInfo &lhs, const ObjCMethodInfo &rhs) {
    return !(lhs == rhs);
  }

  /// Merge class-wide information into the given method.
  friend ObjCMethodInfo &operator|=(ObjCMethodInfo &lhs,
                                    const ObjCContextInfo &rhs) {
    // Merge nullability.
    if (!lhs.NullabilityAudited) {
      if (auto nullable = rhs.getDefaultNullability()) {
        lhs.NullabilityAudited = true;
        lhs.addTypeInfo(0, *nullable);
      }
    }

    return lhs;
  }

  void dump(llvm::raw_ostream &os);
};

/// Describes API notes data for a global variable.
class GlobalVariableInfo : public VariableInfo {
public:
  GlobalVariableInfo() : VariableInfo() { }
};

/// Describes API notes data for a global function.
class GlobalFunctionInfo : public FunctionInfo {
public:
  GlobalFunctionInfo() : FunctionInfo() { }
};
} // end namespace api_notes
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_TYPES_H
