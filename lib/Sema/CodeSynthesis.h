//===--- CodeSynthesis.h - Typechecker code synthesis -----------*- C++ -*-===//
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
//  This file defines a typechecker-internal interface to a bunch of
//  routines for synthesizing various declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPECHECKING_CODESYNTHESIS_H
#define SWIFT_TYPECHECKING_CODESYNTHESIS_H

#include "swift/AST/ForeignErrorConvention.h"
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

// These are implemented in TypeCheckDecl.cpp.
void makeFinal(ASTContext &ctx, ValueDecl *D);
void makeDynamic(ASTContext &ctx, ValueDecl *D);
void markAsObjC(TypeChecker &TC, ValueDecl *D, bool isObjC,
                Optional<ForeignErrorConvention> errorConvention = None);
Type configureImplicitSelf(TypeChecker &tc,
                           AbstractFunctionDecl *func,
                           GenericParamList *&outerGenericParams);
void configureConstructorType(ConstructorDecl *ctor,
                              GenericParamList *outerGenericParams,
                              Type selfType,
                              Type argType,
                              bool throws);
bool checkOverrides(TypeChecker &TC, ValueDecl *decl);

// These are implemented in CodeSynthesis.cpp.
void convertStoredVarInProtocolToComputed(VarDecl *VD, TypeChecker &TC);
void synthesizeObservingAccessors(VarDecl *VD, TypeChecker &TC);
void synthesizeSetterForMutableAddressedStorage(AbstractStorageDecl *storage,
                                                TypeChecker &TC);
void synthesizeMaterializeForSet(FuncDecl *materializeForSet,
                                 AbstractStorageDecl *storage,
                                 TypeChecker &TC);
void maybeAddMaterializeForSet(AbstractStorageDecl *storage,
                               TypeChecker &TC);

void addTrivialAccessorsToStorage(AbstractStorageDecl *storage,
                                  TypeChecker &TC);

void maybeAddAccessorsToVariable(VarDecl *var, TypeChecker &TC);

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
