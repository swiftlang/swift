//===--- Actor.h - Swift concurrency actor declarations ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Actor-related declarations that are also used by Remote Mirror.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_ACTOR_H
#define SWIFT_CONCURRENCY_ACTOR_H

#include <stdint.h>

namespace swift {
namespace concurrency {
namespace ActorFlagConstants {

enum : uint32_t {
  // Bits 0-2: Actor state
  //
  // Possible state transitions for an actor:
  //
  // Idle -> Running
  // Running -> Idle
  // Running -> Scheduled
  // Scheduled -> Running
  // Idle -> Deallocated
  // Running -> Zombie_ReadyForDeallocation
  // Zombie_ReadyForDeallocation -> Deallocated
  //
  // It is possible for an actor to be in Running and yet completely released
  // by clients. However, the actor needs to be kept alive until it is done
  // executing the task that is running on it and gives it up. It is only
  // after that we can safely deallocate it.
  ActorStateMask = 0x7,

  /// The actor is not currently scheduled.  Completely redundant
  /// with the job list being empty.
  Idle = 0x0,
  /// There actor is scheduled
  Scheduled = 0x1,
  /// There is currently a thread running the actor.
  Running = 0x2,
  /// The actor is ready for deallocation once it stops running
  Zombie_ReadyForDeallocation = 0x3,

  // Bit 3
  DistributedRemote = 0x8,
  // Bit 4
  IsPriorityEscalated = 0x10,

  // Bits 8 - 15. We only need 8 bits of the whole size_t to represent Job
  // Priority
  PriorityMask = 0xFF00,
  PriorityAndOverrideMask = PriorityMask | IsPriorityEscalated,
  PriorityShift = 0x8,
};

} // namespace ActorFlagConstants
} // namespace concurrency
} // namespace swift

#endif // SWIFT_CONCURRENCY_ACTOR_H
