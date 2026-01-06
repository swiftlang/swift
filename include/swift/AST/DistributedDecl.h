//===-- DistributedDecl.h - Distributed declaration utils -------*- C++ -*-===//
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
// This file provides functions for working with declarations of distributed
// actors and declarations related to them, like associated types and protocols.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECL_DISTRIBUTEDDECL_H
#define SWIFT_DECL_DISTRIBUTEDDECL_H

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

Type getAssociatedTypeOfDistributedSystemOfActor(DeclContext *actorOrExtension,
                                                 Identifier member);

/// Find the concrete invocation decoder associated with the given actor.
NominalTypeDecl *getDistributedActorInvocationDecoder(NominalTypeDecl *);

/// Determine if this distributed actor can synthesize a `Codable` conformance.
/// This is based on the actor's `ID` being `Codable`.
///
///  It is possible for the `ID` to be `Codable` but the
/// `SerializationRequirement` used by the actor (and its actor system to not
/// be `Codable`). In such situation the conformance is synthesized, however
/// the user may need to provide an explicit conformance to the
/// `SerializationRequirement` if they wanted to pass the actor to distributed
/// methods.
bool canSynthesizeDistributedActorCodableConformance(NominalTypeDecl *actor);

/// Find `decodeNextArgument<T>(type: T.Type) -> T` method associated with
/// invocation decoder of the given distributed actor.
FuncDecl *getDistributedActorArgumentDecodingMethod(NominalTypeDecl *);

/// Determine the concrete type of 'ActorSystem' as seen from the member.
/// E.g. when in a protocol, and trying to determine what the actor system was
/// constrained to.
///
/// \param member the member from which context the lookup should be performed,
///        e.g. a function or computed property.
/// \return the concrete type of the ActorSystem to be used by this member,
///         or null if no concrete actor system was found.
Type getConcreteReplacementForProtocolActorSystemType(ValueDecl *member);

/// Determine the `ActorSystem` type for the given actor.
Type getDistributedActorSystemType(NominalTypeDecl *actor);

/// Determine the `ID` type for the given actor.
Type getDistributedActorIDType(NominalTypeDecl *actor);

/// Get specific 'SerializationRequirement' as defined in 'nominal'
/// type, which must conform to the passed 'protocol' which is expected
/// to require the 'SerializationRequirement'.
Type getDistributedSerializationRequirementType(
    NominalTypeDecl *nominal, ProtocolDecl *protocol);

/// Given a distributed thunk declaration, inside a 'distributed actor',
/// finds the ad-hoc witness for 'decodeNextArgument' on the associated
/// 'ActorSystem.InvocationDecoder' of the actor, or null.
AbstractFunctionDecl *
getAssociatedDistributedInvocationDecoderDecodeNextArgumentFunction(
    ValueDecl *thunk);

Type getDistributedActorSerializationType(DeclContext *actorOrExtension);

/// Get the specific 'SerializationRequirement' type of a specific distributed
/// actor system.
Type getDistributedActorSystemSerializationType(NominalTypeDecl *system);

/// Get the specific 'InvocationEncoder' type of a specific distributed actor
/// system.
Type getDistributedActorSystemInvocationEncoderType(NominalTypeDecl *system);

/// Get the specific 'InvocationDecoder' type of a specific distributed actor
/// system.
Type getDistributedActorSystemInvocationDecoderType(NominalTypeDecl *system);

/// Get the specific 'ResultHandler' type of a specific distributed actor
/// system.
Type getDistributedActorSystemResultHandlerType(NominalTypeDecl *system);

/// Get the 'ActorID' type of a specific distributed actor system.
Type getDistributedActorSystemActorIDType(NominalTypeDecl *system);

/// Retrieve a protocol conformance to the `Actor` protocol for a
/// distributed actor type that is described via a substitution map for
/// the generic signature `<T: DistributedActor>`.
///
/// The protocol conformance is a special one that is currently
/// only used by the `distributedActorAsAnyActor` builtin.
NormalProtocolConformance *
getDistributedActorAsActorConformance(ASTContext &C);

ProtocolConformanceRef
getDistributedActorAsActorConformanceRef(ASTContext &C);

/// Find the extension that defines the methods necessary for creating the
/// the DistributedActor-as-Actor conformance.
ExtensionDecl *
findDistributedActorAsActorExtension(
    ProtocolDecl *distributedActorProto);

bool isDistributedActorAsLocalActorComputedProperty(VarDecl *var);

/// Get the ``DistributedActor/asLocalActor`` computed property.
VarDecl *getDistributedActorAsLocalActorComputedProperty(ModuleDecl *module);

/// Check if the `allRequirements` represent *exactly* the
/// `Encodable & Decodable` (also known as `Codable`) requirement.
///
/// If so, we can emit slightly nicer diagnostics.
bool checkDistributedSerializationRequirementIsExactlyCodable(
    ASTContext &C,
    Type type);

/// Get the `SerializationRequirement`, explode it into the specific
/// protocol requirements and insert them into `requirements`.
///
/// The passed `protocol` must be conformed to by the `decl`, e.g. a specific
/// actor system implementation and the `DistributedActorSystem` protocol,
/// or any of the specific encoder/decoder and the respective
/// Distributed...Encoder/Decoder protocol etc.
///
/// Returns false if failed to get the protocol decls.
bool
getDistributedSerializationRequirements(
    NominalTypeDecl *decl,
    ProtocolDecl *protocol,
    llvm::SmallPtrSetImpl<ProtocolDecl *> &requirementProtos);

/// Retrieve the declaration of DistributedActorSystem.remoteCall(Void)(...).
///
/// \param actorOrSystem distributed actor or actor system to get the
/// remoteCall function for. Since the method we're looking for is an ad-hoc
/// requirement, a specific type MUST be passed here as it is not possible
/// to obtain the decl from just the `DistributedActorSystem` protocol type.
/// \param isVoidReturn true if the call will be returning `Void`.
AbstractFunctionDecl *
getRemoteCallOnDistributedActorSystem(NominalTypeDecl *actorOrSystem,
                                      bool isVoidReturn);

/// Retrieve the declaration of DistributedActorSystem.make().
///
/// \param thunk the function from which we'll be invoking things on the
/// obtained actor system; This way we'll always get the right type, taking care
/// of any where clauses etc.
FuncDecl *
getMakeInvocationEncoderOnDistributedActorSystem(AbstractFunctionDecl *thunk);

// Retrieve the declaration of
// DistributedInvocationEncoder.recordGenericSubstitution(_:).
//
// \param nominal optionally provide a 'NominalTypeDecl' from which the
// function decl shall be extracted. This is useful to avoid witness calls
// through the protocol which is looked up when nominal is null.
FuncDecl *getRecordGenericSubstitutionOnDistributedInvocationEncoder(
    NominalTypeDecl *nominal);

// Retrieve the declaration of
// DistributedTargetInvocationEncoder.recordArgument(_:).
//
// \param nominal optionally provide a 'NominalTypeDecl' from which the
// function decl shall be extracted. This is useful to avoid witness calls
// through the protocol which is looked up when nominal is null.
AbstractFunctionDecl *
getRecordArgumentOnDistributedInvocationEncoder(NominalTypeDecl *nominal);

// Retrieve the declaration of
// DistributedTargetInvocationEncoder.recordReturnType(_:).
AbstractFunctionDecl *
getRecordReturnTypeOnDistributedInvocationEncoder(NominalTypeDecl *nominal);

// Retrieve the declaration of
// DistributedTargetInvocationEncoder.recordErrorType(_:).
AbstractFunctionDecl *
getRecordErrorTypeOnDistributedInvocationEncoder(NominalTypeDecl *nominal);

// Retrieve the declaration of
// DistributedTargetInvocationDecoder.getDecodeNextArgumentOnDistributedInvocationDecoder(_:).
AbstractFunctionDecl *
getDecodeNextArgumentOnDistributedInvocationDecoder(NominalTypeDecl *nominal);

// Retrieve the declaration of
// getOnReturnOnDistributedTargetInvocationResultHandler.onReturn(_:).
AbstractFunctionDecl *
getOnReturnOnDistributedTargetInvocationResultHandler(NominalTypeDecl *nominal);

// Retrieve the declaration of DistributedInvocationEncoder.doneRecording().
//
// \param nominal optionally provide a 'NominalTypeDecl' from which the
// function decl shall be extracted. This is useful to avoid witness calls
// through the protocol which is looked up when nominal is null.
FuncDecl *
getDoneRecordingOnDistributedInvocationEncoder(NominalTypeDecl *nominal);
}

#endif /* SWIFT_DECL_DISTRIBUTEDDECL_H */
