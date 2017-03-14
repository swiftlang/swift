//===--- OverloadChoice.h - A Choice from an Overload Set  ------*- C++ -*-===//
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
// This file provides the \c OverloadChoice class and its related types,
// which is used by the constraint-based type checker to describe the
// selection of a particular overload from a set.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_OVERLOADCHOICE_H
#define SWIFT_SEMA_OVERLOADCHOICE_H

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"
#include "swift/AST/Availability.h"
#include "swift/AST/FunctionRefKind.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"

namespace swift {

class TypeDecl;
class ValueDecl;

namespace constraints {
  class ConstraintSystem;

/// \brief The kind of overload choice.
enum class OverloadChoiceKind : int {
  /// \brief The overload choice selects a particular declaration from a
  /// set of declarations.
  Decl,
  /// \brief The overload choice selects a particular declaration that was
  /// found via dynamic lookup and, therefore, might not actually be
  /// available at runtime.
  DeclViaDynamic,
  /// \brief The overload choice selects a particular declaration from a
  /// set of declarations and treats it as a type.
  TypeDecl,
  /// \brief The overload choice equates the member type with the
  /// base type. Used for unresolved member expressions like ".none" that
  /// refer to enum members with unit type.
  BaseType,
  /// \brief The overload choice indexes into a tuple. Index zero will
  /// have the value of this enumerator, index one will have the value of this
  /// enumerator + 1, and so on. Thus, this enumerator must always be last.
  TupleIndex,
  /// \brief The overload choice selects a particular declaration that
  /// was found by bridging the base value type to its Objective-C
  /// class type.
  DeclViaBridge,
  /// \brief The overload choice selects a particular declaration that
  /// was found by unwrapping an optional context type.
  DeclViaUnwrappedOptional,
  /// \brief The overload choice selects a particular declaration that  
  /// was found by ignoring optional argument labels.
  DeclViaOmittedLabels,
};

/// \brief Describes a particular choice within an overload set.
///
/// 
class OverloadChoice {
  enum : unsigned {
    /// Indicates whether this overload was immediately specialized.
    IsSpecializedBit = 0x01,
    /// Indicates whether this declaration was bridged, turning a
    /// "Decl" kind into "DeclViaBridge" kind.
    IsBridgedBit = 0x02,
    /// Indicates whether this declaration was resolved by unwrapping an
    /// optional context type, turning a "Decl" kind into
    /// "DeclViaUnwrappedOptional".
    IsUnwrappedOptionalBit = 0x04,
    /// Indicates whether this declaration was resolved by ignoring 
    /// optional argument labels.
    OmitsLabelsBits = IsBridgedBit | IsUnwrappedOptionalBit,
  };

  /// \brief The base type to be used when referencing the declaration
  /// along with two bits: the low bit indicates whether this overload
  /// was immediately specialized and the second lowest bit indicates
  /// whether the declaration was bridged.
  llvm::PointerIntPair<Type, 3, unsigned> BaseAndBits;

  /// \brief Either the declaration pointer (if the low bit is clear) or the
  /// overload choice kind shifted two bits with the low bit set.
  uintptr_t DeclOrKind;

  /// The kind of function reference.
  /// FIXME: This needs two bits. Can we pack them somewhere?
  FunctionRefKind TheFunctionRefKind;

public:
  OverloadChoice()
    : BaseAndBits(nullptr, 0), DeclOrKind(),
      TheFunctionRefKind(FunctionRefKind::Unapplied) {}

  OverloadChoice(Type base, ValueDecl *value, bool isSpecialized,
                 FunctionRefKind functionRefKind)
    : BaseAndBits(base, isSpecialized ? IsSpecializedBit : 0),
      TheFunctionRefKind(functionRefKind) {
    assert(!base || !base->hasTypeParameter());
    assert((reinterpret_cast<uintptr_t>(value) & (uintptr_t)0x03) == 0 &&
           "Badly aligned decl");
    
    DeclOrKind = reinterpret_cast<uintptr_t>(value);
  }
  
  OverloadChoice(Type base, TypeDecl *type, bool isSpecialized,
                 FunctionRefKind functionRefKind)
    : BaseAndBits(base, isSpecialized ? IsSpecializedBit : 0),
      TheFunctionRefKind(functionRefKind) {
    assert(!base || !base->hasTypeParameter());
    assert((reinterpret_cast<uintptr_t>(type) & (uintptr_t)0x03) == 0
           && "Badly aligned decl");
    DeclOrKind = reinterpret_cast<uintptr_t>(type) | 0x01;
  }

  OverloadChoice(Type base, OverloadChoiceKind kind)
      : BaseAndBits(base, 0),
        DeclOrKind((uintptr_t)kind << 2 | (uintptr_t)0x03),
        TheFunctionRefKind(FunctionRefKind::Unapplied) {
    assert(base && "Must have a base type for overload choice");
    assert(!base->hasTypeParameter());
    assert(kind != OverloadChoiceKind::Decl &&
           kind != OverloadChoiceKind::DeclViaDynamic &&
           kind != OverloadChoiceKind::TypeDecl &&
           kind != OverloadChoiceKind::DeclViaBridge &&
           kind != OverloadChoiceKind::DeclViaUnwrappedOptional &&
           "wrong constructor for decl");
  }

  OverloadChoice(Type base, unsigned index)
      : BaseAndBits(base, 0),
        DeclOrKind(((uintptr_t)index
                    + (uintptr_t)OverloadChoiceKind::TupleIndex) << 2
                    | (uintptr_t)0x03),
        TheFunctionRefKind(FunctionRefKind::Unapplied) {
    assert(base->getRValueType()->is<TupleType>() && "Must have tuple type");
  }

  /// Retrieve an overload choice for a declaration that was found via
  /// dynamic lookup.
  static OverloadChoice getDeclViaDynamic(Type base, ValueDecl *value,
                                          FunctionRefKind functionRefKind) {
    OverloadChoice result;
    result.BaseAndBits.setPointer(base);
    result.DeclOrKind = reinterpret_cast<uintptr_t>(value) | 0x02;
    result.TheFunctionRefKind = functionRefKind;
    return result;
  }

  /// Retrieve an overload choice for a declaration that was found via
  /// bridging to an Objective-C class.
  static OverloadChoice getDeclViaBridge(Type base, ValueDecl *value,
                                         FunctionRefKind functionRefKind) {
    OverloadChoice result;
    result.BaseAndBits.setPointer(base);
    result.BaseAndBits.setInt(IsBridgedBit);
    result.DeclOrKind = reinterpret_cast<uintptr_t>(value);
    result.TheFunctionRefKind = functionRefKind;
    return result;
  }

  /// Retrieve an overload choice for a declaration that was found
  /// by unwrapping an optional context type.
  static OverloadChoice getDeclViaUnwrappedOptional(
      Type base,
      ValueDecl *value,
      FunctionRefKind functionRefKind) {
    OverloadChoice result;
    result.BaseAndBits.setPointer(base);
    result.BaseAndBits.setInt(IsUnwrappedOptionalBit);
    result.DeclOrKind = reinterpret_cast<uintptr_t>(value);
    result.TheFunctionRefKind = functionRefKind;
    return result;
  }
  
  /// Retrieve an overload choice for a declaration that was found via
  /// dynamic lookup.
  static OverloadChoice getDeclViaOmittedLabels(Type base, ValueDecl *value,
                                          FunctionRefKind functionRefKind) {
    OverloadChoice result;
    result.BaseAndBits.setPointer(base);
    result.BaseAndBits.setInt(OmitsLabelsBits);
    result.DeclOrKind = reinterpret_cast<uintptr_t>(value);
    result.TheFunctionRefKind = functionRefKind;
    return result;
  }

  /// \brief Retrieve the base type used to refer to the declaration.
  Type getBaseType() const { return BaseAndBits.getPointer(); }

  /// \brief Determine whether the referenced declaration was immediately
  /// specialized with <...>.
  ///
  /// This value only has meaning when there is no base type.
  bool isSpecialized() const { 
    return BaseAndBits.getInt() & IsSpecializedBit;
  }
  
  /// \brief Determines the kind of overload choice this is.
  OverloadChoiceKind getKind() const {
    switch (DeclOrKind & 0x03) {
    case 0x00: 
      if ((BaseAndBits.getInt() & OmitsLabelsBits) == OmitsLabelsBits)
        return OverloadChoiceKind::DeclViaOmittedLabels;
      if (BaseAndBits.getInt() & IsBridgedBit)
        return OverloadChoiceKind::DeclViaBridge;
      if (BaseAndBits.getInt() & IsUnwrappedOptionalBit)
        return OverloadChoiceKind::DeclViaUnwrappedOptional;

      return OverloadChoiceKind::Decl;
      
    case 0x01: return OverloadChoiceKind::TypeDecl;
    case 0x02: return OverloadChoiceKind::DeclViaDynamic;
    case 0x03: {
      uintptr_t value = DeclOrKind >> 2;
      if (value >= (uintptr_t)OverloadChoiceKind::TupleIndex)
        return OverloadChoiceKind::TupleIndex;

      return (OverloadChoiceKind)value;
    }

    default: llvm_unreachable("basic math has escaped me");
    }
  }

  /// Determine whether this choice is for a declaration.
  bool isDecl() const {
    switch (getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::TypeDecl:
    case OverloadChoiceKind::DeclViaBridge:
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
    case OverloadChoiceKind::DeclViaOmittedLabels:
      return true;

    case OverloadChoiceKind::BaseType:
    case OverloadChoiceKind::TupleIndex:
      return false;
    }

    llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
  }

  /// \brief Retrieve the declaration that corresponds to this overload choice.
  ValueDecl *getDecl() const {
    assert(isDecl() && "Not a declaration");
    return reinterpret_cast<ValueDecl *>(DeclOrKind & ~(uintptr_t)0x03);
  }

  /// \brief Retrieve the tuple index that corresponds to this overload
  /// choice.
  unsigned getTupleIndex() const {
    assert(getKind() == OverloadChoiceKind::TupleIndex);
    return (DeclOrKind >> 2) - (uintptr_t)OverloadChoiceKind::TupleIndex;
  }

  /// \brief Retrieves an opaque choice that ignores the base type.
  void *getOpaqueChoiceSimple() const {
    return reinterpret_cast<void*>(DeclOrKind);
  }

  FunctionRefKind getFunctionRefKind() const {
    assert(isDecl() && "only makes sense for declaration choices");
    return TheFunctionRefKind;
  }
};

} // end namespace constraints
} // end namespace swift

#endif // LLVM_SWIFT_SEMA_OVERLOADCHOICE_H
