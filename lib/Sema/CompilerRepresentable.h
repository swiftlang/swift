//===--- CompilerRepresentable.h - Check compiler evaluability of types ---===//
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
// SWIFT_ENABLE_TENSORFLOW
// Checks if the compile-time evaluator can handle values of a type.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"

namespace swift {

class UnrepresentableReason;
typedef std::unique_ptr<const UnrepresentableReason> UnrepresentableReasonPtr;

/// Stores the reason why the compile-time evaluator cannot handle a type.
class UnrepresentableReason {
private:
  enum UnrepresentableKind {
    // It's unrepresentable because it's a class.
    // UnderlyingReason is nullptr.
    IsClass,

    // It has a kind that we do not know how to handle.
    // UnderlyingReason is nullptr.
    UnknownTypeKind,

    // It is the metatype of a unrepresentable type.
    // UnderlyingReason is the reason that that type is unrepresentable.
    MetatypeOfUnrepresentable,

    // It is a bound generic type, and one of the arguments is unrepresentable.
    // UnderlyingReason is the reason that the argument is unrepresentable.
    GenericArgUnrepresentable,

    // It is an enum with an unrepresentable element.
    // UnderlyingReason is the reason that the element is unrepresentable.
    EnumElementUnrepresentable,

    // It has a field with unrepresentable type.
    // UnderlyingReason is the reason that the field type is unrepresentable.
    FieldUnrepresentable,

    // It has a tuple element of unrepresentable type.
    // UnderlyingReason is the reason that the element type is unrepresentable.
    TupleElementUnrepresentable,

    // It is a function type with an unrepresentable parameter type.
    // UnderlyingReason is the reason that the parameter type is
    // unrepresentable.
    ParamUnrepresentable,

    // It is a function type with an unrepresentable result type.
    // UnderlyingReason is the reason that the result type is unrepresentable.
    ResultUnrepresentable,
  };

  UnrepresentableKind Kind;

  union {
    // When the Kind is EnumElementUnrepresentable, this is the declaration of
    // the unrepresentable enum element.
    const EnumElementDecl *UnrepresentableEnumElementDecl;

    // When the kind is FieldUnrepresentable, this is the declaration of the
    // unrepresentable field.
    const VarDecl *UnrepresentableFieldDecl;
  };

  // The type that is not representable.
  Type UnrepresentableType;

  // When the reason says that another type is unrepresentable, this stores the
  // reason that that type is unrepresentable.
  UnrepresentableReasonPtr UnderlyingReason;

  UnrepresentableReason(UnrepresentableKind Kind, Type UnrepresentableType,
                        UnrepresentableReasonPtr UnderlyingReason)
      : Kind(Kind), UnrepresentableType(UnrepresentableType),
        UnderlyingReason(std::move(UnderlyingReason)) {}

  template <class DiagTarget>
  void emitDiagnosticNotesImpl(TypeChecker &TC, DiagTarget Target) const;

public:
  static UnrepresentableReasonPtr getIsClass(Type UnrepresentableType);

  static UnrepresentableReasonPtr getUnknownTypeKind(Type UnrepresentableType);

  static UnrepresentableReasonPtr
  getMetatypeOfUnrepresentable(Type UnrepresentableType,
                               UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr
  getGenericArgUnrepresentable(Type UnrepresentableType,
                               UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr getEnumElementUnrepresentable(
      Type UnrepresentableType,
      const EnumElementDecl *UnrepresentableEnumElementDecl,
      UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr
  getFieldUnrepresentable(Type UnrepresentableType,
                          const VarDecl *UnrepresentableFieldDecl,
                          UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr
  getTupleElementUnrepresentable(Type UnrepresentableType,
                                 UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr
  getParamUnrepresentable(Type UnrepresentableType,
                          UnrepresentableReasonPtr UnderlyingReason);

  static UnrepresentableReasonPtr
  getResultUnrepresentable(Type UnrepresentableType,
                           UnrepresentableReasonPtr UnderlyingReason);

  void emitDiagnosticNotes(TypeChecker &TC, SourceLoc Target) const;
  void emitDiagnosticNotes(TypeChecker &TC, const Decl *Target) const;
};

class CompilerRepresentableChecker {
  // Checks if the compile-time evaluator can handle the types that the generic
  // arguments are bound to.
  UnrepresentableReasonPtr checkGenericArgs(CanBoundGenericType type);

  // Checks if the compile-time evaluator can handle this enum.
  UnrepresentableReasonPtr checkDeclaredType(const EnumDecl *decl);

  // Checks if the compile-time evaluator can handle this struct.
  UnrepresentableReasonPtr checkDeclaredType(const StructDecl *decl);

  // Checks if the compile-time evaluator can handle the types of the parameters
  // of this function.
  UnrepresentableReasonPtr checkParams(CanAnyFunctionType type);

public:
  // Checks if the compile-time evaluator can handle values of type `type`.
  UnrepresentableReasonPtr check(Type type);
};

} // end namespace swift
