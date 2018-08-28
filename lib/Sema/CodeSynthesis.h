//===--- CodeSynthesis.h - Typechecker code synthesis -----------*- C++ -*-===//
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
//  This file defines a typechecker-internal interface to a bunch of
//  routines for synthesizing various declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPECHECKING_CODESYNTHESIS_H
#define SWIFT_TYPECHECKING_CODESYNTHESIS_H

#include "TypeCheckObjC.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/Basic/ExternalUnion.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"

namespace swift {

class AbstractFunctionDecl;
class AbstractStorageDecl;
class ASTContext;
class ClassDecl;
class ConstructorDecl;
class FuncDecl;
class GenericParamList;
class NominalTypeDecl;
class Type;
class ValueDecl;
class VarDecl;

class TypeChecker;

class ObjCReason;

/// A function which needs to have its body synthesized.
///
/// This class exists in expectation that someone will need to add more
/// information to it.
class SynthesizedFunction {
public:
  enum Kind {
    Getter,
    Setter,
    ReadCoroutine,
    ModifyCoroutine,
    LazyGetter,
    LazySetter,
  };

private:
  FuncDecl *Fn;
  Kind K;

  using Members = ExternalUnionMembers<void, VarDecl*>;
  static Members::Index getIndexForKind(Kind kind) {
    switch (kind) {
    case Kind::Getter:
    case Kind::Setter:
    case Kind::ReadCoroutine:
    case Kind::ModifyCoroutine:
      return Members::indexOf<void>();
    case Kind::LazyGetter:
    case Kind::LazySetter:
      return Members::indexOf<VarDecl*>();
    }
    llvm_unreachable("bad kind");
  };
  ExternalUnion<Kind, Members, getIndexForKind> Extra;
  static_assert(decltype(Extra)::union_is_trivially_copyable,
                "expected all members to be trivial");

public:
  SynthesizedFunction(FuncDecl *fn, Kind kind) : Fn(fn), K(kind) {
    assert(getIndexForKind(kind) == Members::indexOf<void>() &&
           "this storage kind requires extra data");
  }

  SynthesizedFunction(FuncDecl *fn, Kind kind, VarDecl *var) : Fn(fn), K(kind) {
    Extra.emplace<VarDecl*>(K, var);
  }

  FuncDecl *getDecl() const { return Fn; }
  Kind getKind() const { return K; }

  VarDecl *getLazyTargetVariable() const { return Extra.get<VarDecl*>(K); }
};

// These are implemented in TypeCheckDecl.cpp.
void makeFinal(ASTContext &ctx, ValueDecl *D);

// Implemented in TypeCheckerOverride.cpp
bool checkOverrides(ValueDecl *decl);

// These are implemented in CodeSynthesis.cpp.
void maybeAddAccessorsToStorage(TypeChecker &TC, AbstractStorageDecl *storage);

void triggerAccessorSynthesis(TypeChecker &TC, AbstractStorageDecl *storage);

/// \brief Describes the kind of implicit constructor that will be
/// generated.
enum class ImplicitConstructorKind {
  /// \brief The default constructor, which default-initializes each
  /// of the instance variables.
  Default,
  /// \brief The memberwise constructor, which initializes each of
  /// the instance variables from a parameter of the same type and
  /// name.
  Memberwise
};

/// \brief Create an implicit struct or class constructor.
///
/// \param decl The struct or class for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct or class).
ConstructorDecl *createImplicitConstructor(TypeChecker &tc,
                                           NominalTypeDecl *decl,
                                           ImplicitConstructorKind ICK);

/// The kind of designated initializer to synthesize.
enum class DesignatedInitKind {
  /// A stub initializer, which is not visible to name lookup and
  /// merely aborts at runtime.
  Stub,

  /// An initializer that simply chains to the corresponding
  /// superclass initializer.
  Chaining
};

/// Create a new initializer that overrides the given designated
/// initializer.
///
/// \param classDecl The subclass in which the new initializer will
/// be declared.
///
/// \param superclassCtor The superclass initializer for which this
/// routine will create an override.
///
/// \param kind The kind of initializer to synthesize.
///
/// \returns the newly-created initializer that overrides \p
/// superclassCtor.
ConstructorDecl *createDesignatedInitOverride(TypeChecker &TC,
                                              ClassDecl *classDecl,
                                              ConstructorDecl *superclassCtor,
                                              DesignatedInitKind kind);

} // end namespace swift

#endif
