//===--- CanTypeVisitor.h - TypeVisitor specialization ----------*- C++ -*-===//
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
// This file defines CanTypeVisitor, a specialized version of
// TypeVisitor for visiting fully type-checked canonical types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CANTYPEVISITOR_H
#define SWIFT_AST_CANTYPEVISITOR_H

#include "swift/AST/Types.h"

namespace swift {

/// This is a specialization of swift::TypeVisitor which:
///   - works only on canonical and fully-checked types and
///   - preserves the canonicality of the visited types in the
///     static (C++) type.
template<typename ImplClass, typename RetTy = void, typename... Args>
class CanTypeVisitor {
public:
  RetTy visit(CanType T, Args... args) {
    switch (T->getKind()) {
#define SUGARED_TYPE(CLASS, PARENT) \
    case TypeKind::CLASS:
#define TYPE(CLASS, PARENT)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("non-canonical type");

#define SUGARED_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT)                                  \
    case TypeKind::CLASS:                                    \
      return static_cast<ImplClass*>(this)                   \
        ->visit##CLASS##Type(cast<CLASS##Type>(T),           \
                             ::std::forward<Args>(args)...);
#include "swift/AST/TypeNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }
  
  // Provide default implementations of abstract "visit" implementations that
  // just chain to their base class.  This allows visitors to just implement
  // the base behavior and handle all subclasses if they desire.  Since this is
  // a template, it will only instantiate cases that are used and thus we still
  // require full coverage of the AST nodes by the visitor.
#define ABSTRACT_TYPE(CLASS, PARENT)                           \
  RetTy visit##CLASS##Type(Can##CLASS##Type T, Args... args) { \
     return static_cast<ImplClass*>(this)                      \
              ->visit##PARENT(T, std::forward<Args>(args)...); \
  }
#define TYPE(CLASS, PARENT) ABSTRACT_TYPE(CLASS, PARENT)
#define ABSTRACT_SUGARED_TYPE(CLASS, PARENT)
#define SUGARED_TYPE(CLASS, PARENT)
  // Don't allow unchecked types by default, but allow visitors to opt-in to
  // handling them.
#define UNCHECKED_TYPE(CLASS, PARENT)                          \
  RetTy visit##CLASS##Type(Can##CLASS##Type T, Args... args) { \
     llvm_unreachable("unchecked type");                       \
  }
#include "swift/AST/TypeNodes.def"
};

/// This is a convenience refinement of CanTypeVisitor which forwards the
/// paired nominal type methods to a common implementation.
///
/// The delegation flow is:
///   {ClassType, BoundGenericClassType} -> AnyClassType -> AnyNominalType -> Type
///   {EnumType, BoundGenericEnumType} -> AnyEnumType -> AnyNominalType -> Type
///   {StructType, BoundGenericStructType} -> AnyStructType -> AnyNominalType -> Type
///   ProtocolType -> AnyNominalType -> Type
///
/// The new visitAny*Type methods take the appropriate Decl* as their second
/// argument.
template<typename ImplClass, typename RetTy = void, typename... Args>
class CanTypeVisitor_AnyNominal : public CanTypeVisitor<ImplClass, RetTy, Args...> {
public:
  RetTy visitClassType(CanClassType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyClassType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitBoundGenericClassType(CanBoundGenericClassType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyClassType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitAnyClassType(CanType T, ClassDecl *D, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyNominalType(T, D, ::std::forward<Args>(args)...);
  }

  RetTy visitStructType(CanStructType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyStructType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitBoundGenericStructType(CanBoundGenericStructType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyStructType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitAnyStructType(CanType T, StructDecl *D, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyNominalType(T, D, ::std::forward<Args>(args)...);
  }

  RetTy visitEnumType(CanEnumType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyEnumType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitBoundGenericEnumType(CanBoundGenericEnumType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyEnumType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }
  RetTy visitAnyEnumType(CanType T, EnumDecl *D, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyNominalType(T, D, ::std::forward<Args>(args)...);
  }

  RetTy visitProtocolType(CanProtocolType T, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitAnyNominalType(T, T->getDecl(), ::std::forward<Args>(args)...);
  }

  RetTy visitAnyNominalType(CanType T, NominalTypeDecl *D, Args... args) {
    return static_cast<ImplClass*>(this)
      ->visitType(T, ::std::forward<Args>(args)...);
  }
};

} // end namespace swift
  
#endif
