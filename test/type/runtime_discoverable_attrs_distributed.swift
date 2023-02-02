// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/RuntimeAttrs.swift -o %t
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature RuntimeDiscoverableAttrs -I %t

// REQUIRES: asserts

import RuntimeAttrs
import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@runtimeMetadata
struct FlagForAsyncFuncs {
  init<Act>(attachedTo: (Act) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int, inout [Int]) async throws -> Void) {}
  init<Act>(attachedTo: (Act, Int) async -> Void) {}
  init(attachedTo: () async -> [String]) {}
}

@ActorFlag
distributed actor TestDistributedActor {
  @FlagForAsyncFuncs distributed func asyncExternallyDist() throws {} // ok

  @FlagForAsyncFuncs distributed func doSomethingDist() async throws {} // ok

  @FlagForAsyncFuncs nonisolated func doSomethingNonisolated(_: Int) async {} // ok

  @FlagForAsyncFuncs distributed func doSomethingDist(_: Int, x: [Int]) async {} // ok
}

@runtimeMetadata
enum ActorFlag<A> {
  case actor(A.Type)
  case distributedActor(A.Type)
}

extension ActorFlag where A: DistributedActor {
  init(attachedTo: A.Type) { self = .distributedActor(attachedTo) }
}
