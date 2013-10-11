//===--- OverloadChoice.h - A Choice from an Overload Set  ------*- C++ -*-===//
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
// This file provides the \c OverloadChoice class and its related types,
// which is used by the constraint-based type checker to describe the
// selection of a particular overload from a set.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_OVERLOADCHOICE_H
#define SWIFT_SEMA_OVERLOADCHOICE_H

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"

namespace swift {

class TypeDecl;
class ValueDecl;

namespace constraints {

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
  /// \brief The overload choice equates the member type with a function
  /// of arbitrary input type whose result type is the base type. Used for
  /// unresolved member expressions like ".internal" that refer to enum
  /// members with non-unit type.
  FunctionReturningBaseType,
  /// \brief The overload choice equates the member type with a function
  /// from the base type to itself.
  IdentityFunction,
  /// \brief The overload choice indexes into a tuple. Index zero will
  /// have the value of this enumerator, index one will have the value of this
  /// enumerator + 1, and so on. Thus, this enumerator must always be last.
  TupleIndex
};

/// \brief Describes a particular choice within an overload set.
///
/// 
class OverloadChoice {
  /// \brief The base type to be used when referencing the declaration
  /// along with a bit indicating whether this overload was immediately
  /// specialized.
  llvm::PointerIntPair<Type, 1, bool> BaseAndSpecialized;

  /// \brief Either the declaration pointer (if the low bit is clear) or the
  /// overload choice kind shifted by 1 with the low bit set.
  uintptr_t DeclOrKind;

public:
  OverloadChoice() : BaseAndSpecialized(nullptr, false), DeclOrKind() { }

  OverloadChoice(Type base, ValueDecl *value, bool isSpecialized)
    : BaseAndSpecialized(base, isSpecialized) {
    assert((reinterpret_cast<uintptr_t>(value) & (uintptr_t)0x03) == 0
           && "Badly aligned decl");
    DeclOrKind = reinterpret_cast<uintptr_t>(value);
  }

  OverloadChoice(Type base, TypeDecl *type, bool isSpecialized)
    : BaseAndSpecialized(base, isSpecialized) {
    assert((reinterpret_cast<uintptr_t>(type) & (uintptr_t)0x03) == 0
           && "Badly aligned decl");
    DeclOrKind = reinterpret_cast<uintptr_t>(type) | 0x01;
  }

  OverloadChoice(Type base, OverloadChoiceKind kind)
    : BaseAndSpecialized(base, false),
      DeclOrKind((uintptr_t)kind << 2 | (uintptr_t)0x03) {
    assert(base && "Must have a base type for overload choice");
    assert(kind != OverloadChoiceKind::Decl && "wrong constructor for decl");
  }

  OverloadChoice(Type base, unsigned index)
    : BaseAndSpecialized(base, false),
      DeclOrKind(((uintptr_t)index
                  + (uintptr_t)OverloadChoiceKind::TupleIndex) << 2
                 | (uintptr_t)0x03) {
    assert(base->getRValueType()->is<TupleType>() && "Must have tuple type");
  }

  /// Retrieve an overload choice for a declaration that was found via
  /// dynamic lookup.
  static OverloadChoice getDeclViaDynamic(Type base, ValueDecl *value) {
    OverloadChoice result;
    result.BaseAndSpecialized.setPointer(base);
    result.DeclOrKind = reinterpret_cast<uintptr_t>(value) | 0x02;
    return result;
  }

  /// \brief Retrieve the base type used to refer to the declaration.
  Type getBaseType() const { return BaseAndSpecialized.getPointer(); }

  /// \brief Determine whether the referenced declaration was immediately
  /// specialized with <...>.
  ///
  /// This value only has meaning when there is no base type.
  bool isSpecialized() const { return BaseAndSpecialized.getInt(); }
  
  /// \brief Determines the kind of overload choice this is.
  OverloadChoiceKind getKind() const {
    switch (DeclOrKind & 0x03) {
    case 0x00: return OverloadChoiceKind::Decl;
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

  /// \brief Retrieve the declaraton that corresponds to this overload choice.
  ValueDecl *getDecl() const {
    assert((getKind() == OverloadChoiceKind::Decl ||
            getKind() == OverloadChoiceKind::DeclViaDynamic ||
            getKind() == OverloadChoiceKind::TypeDecl) && "Not a declaration");
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
};

} } // end namespace swift::constraints

#endif // LLVM_SWIFT_SEMA_OVERLOADCHOICE_H
