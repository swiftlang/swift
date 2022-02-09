//===-- TypeCheckDistributed.h - Distributed actor typechecking -*- C++ -*-===//
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

namespace swift {

class ClassDecl;
class ConstructorDecl;
class Decl;
class DeclContext;
class FuncDecl;
class NominalTypeDecl;

/******************************************************************************/
/********************* Distributed Actor Type Checking ************************/
/******************************************************************************/

// Diagnose an error if the _Distributed module is not loaded.
bool ensureDistributedModuleLoaded(Decl *decl);

/// Check for illegal property declarations (e.g. re-declaring transport or id)
void checkDistributedActorProperties(const ClassDecl *decl);

/// The local and resolve distributed actor constructors have special rules to check.
void checkDistributedActorConstructor(const ClassDecl *decl, ConstructorDecl *ctor);

/// Type-check additional ad-hoc protocol requirements.
/// Ad-hoc requirements are protocol requirements currently not expressible
/// in the Swift type-system.
bool checkDistributedActorSystemAdHocProtocolRequirements(
    ASTContext &Context,
    ProtocolDecl *Proto,
    NormalProtocolConformance *Conformance,
    Type Adoptee,
    bool diagnose);

/// Typecheck a distributed method declaration
bool checkDistributedFunction(FuncDecl *decl, bool diagnose);

/// Typecheck a distributed computed (get-only) property declaration.
/// They are effectively checked the same way as argument-less methods.
bool checkDistributedActorProperty(VarDecl *decl, bool diagnose);

/// Determine the distributed actor transport type for the given actor.
Type getDistributedActorSystemType(NominalTypeDecl *actor);

/// Determine the distributed actor identity type for the given actor.
Type getDistributedActorIDType(NominalTypeDecl *actor);

/// Determine the serialization requirement for the given actor, actor system
/// or other type that has the SerializationRequirement associated type.
Type getDistributedSerializationRequirementType(NominalTypeDecl *nominal);

/// Get the specific protocols that the `SerializationRequirement` specifies,
/// and all parameters / return types of distributed targets must conform to.
///
/// E.g. if a system declares `typealias SerializationRequirement = Codable`
/// then this will return `{encodableProtocol, decodableProtocol}`.
///
/// Returns an empty set if the requirement was `Any`.
llvm::SmallPtrSet<ProtocolDecl *, 2>
getDistributedSerializationRequirementProtocols(NominalTypeDecl *decl);

llvm::SmallPtrSet<ProtocolDecl *, 2>
flattenDistributedSerializationTypeToRequiredProtocols(
    TypeBase *serializationRequirement);

/// Check if the `allRequirements` represent *exactly* the
/// `Encodable & Decodable` (also known as `Codable`) requirement.
/// If so, we can emit slightly nicer diagnostics.
bool checkDistributedSerializationRequirementIsExactlyCodable(
    ASTContext &C,
    const llvm::SmallPtrSetImpl<ProtocolDecl *> &allRequirements);

/// Given any set of generic requirements, locate those which are about the
/// `SerializationRequirement`. Those need to be applied in the parameter and
/// return type checking of distributed targets.
llvm::SmallPtrSet<ProtocolDecl *, 2>
extractDistributedSerializationRequirements(
    ASTContext &C, ArrayRef<Requirement> allRequirements);

/// Diagnose a distributed func declaration in a not-distributed actor protocol.
void diagnoseDistributedFunctionInNonDistributedActorProtocol(
  const ProtocolDecl *proto, InFlightDiagnostic &diag);

/// Emit a FixIt suggesting to add Codable to the nominal type.
void addCodableFixIt(const NominalTypeDecl *nominal, InFlightDiagnostic &diag);

}

#endif /* SWIFT_SEMA_TYPECHECKDISTRIBUTED_H */
