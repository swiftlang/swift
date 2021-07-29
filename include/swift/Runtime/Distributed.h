//===--- Distributed.h - Runtime interface for distributed ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The runtime interface for distributed.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_DISTRIBUTED_H
#define SWIFT_RUNTIME_DISTRIBUTED_H

#include "swift/ABI/Actor.h"
//#include "swift/ABI/Task.h"
//#include "swift/ABI/TaskStatus.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

namespace swift {
class DefaultActor;

/// Initialize the runtime storage for a distributed local actor.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_distributedActor_local_initialize(DefaultActor *actor,
                                             OpaqueValue *identity,
                                             OpaqueValue *transport);

/// Initialize the runtime storage for a distributed remote actor.
// TODO: this may end up being removed as we move to the "proxy creation" below
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_distributedActor_remote_initialize(DefaultActor *actor);

/// Create a proxy object that will serve as remote distributed actor instance.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
OpaqueValue* swift_distributedActor_remote_create(
  /* +1 */OpaqueValue *identity,
  /* +1 */OpaqueValue *transport,
  const Metadata *identityType,
  const Metadata *transportType
);

/// Destroy the runtime storage for a default actor.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_distributedActor_destroy(DefaultActor *actor);

/// Extract the `ActorIdentity` from the actors implicit distributed storage.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
OpaqueValue* swift_distributedActor_getIdentity(DefaultActor *actor);

/// Extract the `ActorTransport` from the actors implicit distributed storage.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
OpaqueValue* swift_distributedActor_getTransport(DefaultActor *actor);

/// Check if the actor is a distributed 'remote' actor instance.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_distributed_actor_is_remote(DefaultActor *actor);

}

#endif
