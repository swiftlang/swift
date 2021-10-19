//===---- DistributedActor.h - SIL utils for distributed actors -*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_DISTRIBUTED_ACTOR_H
#define SWIFT_SILOPTIMIZER_UTILS_DISTRIBUTED_ACTOR_H

namespace swift {

class ASTContext;
class ConstructorDecl;
class ClassDecl;
class SILBuilder;
class SILArgument;
class SILFunction;
class SILLocation;
class SILValue;

/// Perform a load of the given distributed actor's transport.
/// \param actorSelf the value representing `self` for the distributed actor
/// instance. \returns the transport value
SILValue loadActorTransport(SILBuilder &B, SILLocation loc,
                            ClassDecl *actorDecl, SILValue actorSelf);

/// Emits code that notifies the distributed actor's transport that the
/// actor is ready for execution.
/// \param B the builder to use when emitting the code.
/// \param actor the distributed actor instance to pass to the transport as
/// being "ready" \param transport a value representing the ActorTransport
void emitActorReadyCall(SILBuilder &B, SILLocation loc, ClassDecl *actorDecl,
                        SILValue actor, SILValue transport);

} // namespace swift

#endif
