// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name main %s
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name main %s -default-class-allocator main.Pool -verify-additional-prefix default-

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: concurrency
// REQUIRES: distributed

// Distributed actors can't have class allocators: the runtime allocates their
// remote proxies with the system allocator. Distributed actors can't be
// subclassed, so they never inherit one. Other actors may have one.

import Distributed

enum Pool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

@allocator(Pool)
distributed actor Greeter { // expected-error@-1 {{'@allocator' cannot be applied to distributed actor 'Greeter' because the runtime allocates its remote proxies}}
  typealias ActorSystem = LocalTestingDistributedActorSystem
}

@allocator(Pool)
distributed actor GreeterOnMainActor { // expected-error@-1 {{'@allocator' cannot be applied to distributed actor 'GreeterOnMainActor' because the runtime allocates its remote proxies}}
  typealias ActorSystem = LocalTestingDistributedActorSystem
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }
}

// The opt-out is allowed.
@allocator(SystemClassAllocator)
distributed actor OptedOut {
  typealias ActorSystem = LocalTestingDistributedActorSystem
}

// Under -default-class-allocator, distributed actors must opt out.
distributed actor Defaulted { // expected-default-error {{default class allocator 'Pool' cannot be applied to distributed actor 'Defaulted' because the runtime allocates its remote proxies}}
  // expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}
  typealias ActorSystem = LocalTestingDistributedActorSystem
}

// Actors that aren't distributed get the default.
actor LocalActor {}

@allocator(Pool)
actor LocalActorWithAllocator {}
