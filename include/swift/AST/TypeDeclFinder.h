//===--- TypeDeclFinder.h - Finds TypeDecls in Types/TypeReprs --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_TYPEDECLFINDER_H
#define SWIFT_AST_TYPEDECLFINDER_H

#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class BoundGenericType;
class DeclRefTypeRepr;
class NominalType;
class TypeAliasType;

/// Walks a Type to find all NominalTypes, BoundGenericTypes, and
/// TypeAliasTypes.
class TypeDeclFinder : public TypeWalker {
  Action walkToTypePre(Type T) override;

public:
  virtual Action visitNominalType(NominalType *ty) {
    return Action::Continue;
  }
  virtual Action visitBoundGenericType(BoundGenericType *ty) {
    return Action::Continue;
  }
  virtual Action visitTypeAliasType(TypeAliasType *ty) {
    return Action::Continue;
  }
};

/// A TypeDeclFinder for use cases where all types should be treated
/// equivalently and where generic arguments can be walked to separately from
/// the generic type.
class SimpleTypeDeclFinder : public TypeDeclFinder {
  /// The function to call when a \c TypeDecl is seen.
  llvm::function_ref<Action(const TypeDecl *)> Callback;

  Action visitNominalType(NominalType *ty) override;
  Action visitBoundGenericType(BoundGenericType *ty) override;
  Action visitTypeAliasType(TypeAliasType *ty) override;

public:
  explicit SimpleTypeDeclFinder(
      llvm::function_ref<Action(const TypeDecl *)> callback)
    : Callback(callback) {}
};

/// Walks a `TypeRepr` and reports all `DeclRefTypeRepr` nodes with bound
/// type declarations by invoking a given callback. These nodes are reported in
/// depth- and base-first AST order. For example, nodes in `A<T>.B<U>` will be
/// reported in the following order: `TAUB`.
class DeclRefTypeReprFinder : public ASTWalker {
  /// The function to call when a `DeclRefTypeRepr` is seen.
  llvm::function_ref<bool(const DeclRefTypeRepr *)> Callback;

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  PostWalkAction walkToTypeReprPost(TypeRepr *TR) override;

public:
  explicit DeclRefTypeReprFinder(
      llvm::function_ref<bool(const DeclRefTypeRepr *)> callback)
      : Callback(callback) {}
};
}

#endif
