//===--- TypeCheckDistributed.h - Distributed -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides type checking support for Swift's distributed actor model.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKDISTRIBUTED_H
#define SWIFT_SEMA_TYPECHECKDISTRIBUTED_H

#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Type.h"
#include <cassert>

namespace swift {

class AbstractFunctionDecl;
class ConstructorDecl;
class ActorIsolation;
class ASTContext;
class ClassDecl;
class Decl;
class DeclContext;
class FuncDecl;
class Initializer;
class PatternBindingDecl;
class ProtocolConformance;
class TopLevelCodeDecl;
class TypeBase;
class ValueDecl;

/******************************************************************************/
/******************** DISTRIBUTED ACTOR TYPECHECKING **************************/
/******************************************************************************/

/// The local and resolve distributed actor constructors have special rules to check.
void checkDistributedActorConstructor(ClassDecl *decl, ConstructorDecl *ctor);

/// Typecheck distributed function, e.g. if parameters conform to right protocol
bool checkDistributedFunction(FuncDecl *decl, bool diagnose);

/******************************************************************************/
/******************** DISTRIBUTED ACTOR TYPECHECKING **************************/
/******************************************************************************/

/// Synthesize the default distributed actor initializer, if necessary.
void addImplicitConstructorsToDistributedActor(ClassDecl *decl);

/// Synthesis of members which are not directly driven filling in protocol requirements,
/// such as the default local and resolve constructors, and `_remote_` function stubs.
void addImplicitDistributedActorMembersToClass(ClassDecl *decl);

}


#endif /* SWIFT_SEMA_TYPECHECKDISTRIBUTED_H */
