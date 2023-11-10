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

/// Similar to `getDistributedSerializationRequirementType`, however, from the
/// perspective of a concrete function. This way we're able to get the
/// serialization requirement for specific members, also in protocols.
Type getConcreteReplacementForMemberSerializationRequirement(ValueDecl *member);

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

/// Get the specific protocols that the `SerializationRequirement` specifies,
/// and all parameters / return types of distributed targets must conform to.
///
/// E.g. if a system declares `typealias SerializationRequirement = Codable`
/// then this will return `{encodableProtocol, decodableProtocol}`.
///
/// Returns an empty set if the requirement was `Any`.
llvm::SmallPtrSet<ProtocolDecl *, 2>
getDistributedSerializationRequirementProtocols(
    NominalTypeDecl *decl, ProtocolDecl* protocol);

/// Check if the `allRequirements` represent *exactly* the
/// `Encodable & Decodable` (also known as `Codable`) requirement.
///
/// If so, we can emit slightly nicer diagnostics.
bool checkDistributedSerializationRequirementIsExactlyCodable(
    ASTContext &C,
    const llvm::SmallPtrSetImpl<ProtocolDecl *> &allRequirements);

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

/// Given any set of generic requirements, locate those which are about the
/// `SerializationRequirement`. Those need to be applied in the parameter and
/// return type checking of distributed targets.
llvm::SmallPtrSet<ProtocolDecl *, 2>
extractDistributedSerializationRequirements(
    ASTContext &C, ArrayRef<Requirement> allRequirements);

}

// ==== ------------------------------------------------------------------------

#endif /* SWIFT_DECL_DISTRIBUTEDDECL_H */
