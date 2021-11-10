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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include <utility>

namespace swift {

class ASTContext;
class ConstructorDecl;
class ClassDecl;
class DeclName;
class SILBasicBlock;
class SILBuilder;
class SILArgument;
class SILFunction;
class SILLocation;
class SILType;
class SILValue;

/// Finds the first `ActorTransport`-compatible parameter of the given function.
/// \returns nullptr if the function does not have such a parameter.
SILArgument *findFirstActorTransportArg(SILFunction &F);

/// Emit a call to a witness of the actor transport protocol.
///
/// \param methodName The name of the method on the ActorTransport protocol.
/// \param transport The transport on which to invoke the method
/// \param actorType If non-empty, the type of the distributed actor that is
/// provided as one of the arguments.
/// \param args The arguments provided to the call, not including the transport.
/// \param tryTargets For a call that can throw, the normal and error basic
/// blocks that the call will branch to.
void emitActorTransportWitnessCall(
    SILBuilder &B, SILLocation loc, DeclName methodName,
    SILValue transport, SILType actorType, llvm::ArrayRef<SILValue> args,
    llvm::Optional<std::pair<SILBasicBlock *, SILBasicBlock *>> tryTargets =
        llvm::None);

/// Emits code that notifies the distributed actor's transport that the
/// actor is ready for execution.
/// \param B the builder to use when emitting the code.
/// \param actor the distributed actor instance to pass to the transport as
/// being "ready" \param transport a value representing the ActorTransport
void emitActorReadyCall(SILBuilder &B, SILLocation loc, SILValue actor,
                        SILValue transport);

} // namespace swift

#endif
