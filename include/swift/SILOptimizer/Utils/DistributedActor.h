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

#include "swift/AST/Decl.h"
#include "llvm/ADT/ArrayRef.h"
#include <optional>
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

/// Creates a reference to the distributed actor's \p actorSystem
/// stored property.
SILValue refDistributedActorSystem(SILBuilder &B,
                                   SILLocation loc,
                                   ClassDecl *actorDecl,
                                   SILValue actorInstance);

/// Emit a call to a witness of the DistributedActorSystem protocol.
///
/// \param methodName The name of the method on the DistributedActorSystem protocol.
/// \param base The base on which to invoke the method
/// \param actorType If non-empty, the type of the distributed actor that is
/// provided as one of the arguments.
/// \param args The arguments provided to the call, not including the base.
/// \param tryTargets For a call that can throw, the normal and error basic
/// blocks that the call will branch to.
void emitDistributedActorSystemWitnessCall(
    SILBuilder &B, SILLocation loc, DeclName methodName, SILValue base,
    SILType actorType, llvm::ArrayRef<SILValue> args,
    std::optional<std::pair<SILBasicBlock *, SILBasicBlock *>> tryTargets =
        std::nullopt);

/// Emits code that notifies the distributed actor's actorSystem that the
/// actor is ready for execution.
/// \param B the builder to use when emitting the code.
/// \param actor the distributed actor instance to pass to the actorSystem as
/// being "ready" \param actorSystem a value representing the DistributedActorSystem
void emitActorReadyCall(SILBuilder &B, SILLocation loc, SILValue actor,
                        SILValue actorSystem);

/// Emits code to notify the \p actorSystem that the given identity is resigned.
void emitResignIdentityCall(SILBuilder &B, SILLocation loc, ClassDecl* actDecl,
                            SILValue actor, SILValue identityRef);


} // namespace swift

#endif
