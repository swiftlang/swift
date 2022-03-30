// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

func nope(test: _local String) {
  // expected-error@-1{{'_local' cannot be used in user-defined code currently}}
  // expected-error@-2{{'_local' parameter has non-distributed-actor type 'String'}}
}

func nopeDA(test: _local DA) {
  // expected-error@-1{{'_local' cannot be used in user-defined code currently}}
}

func nopeDAG<D>(test: _local D) where D: DistributedActor {
  // expected-error@-1{{'_local' cannot be used in user-defined code currently}}
}

struct NotCodable {}

distributed actor DA {
  // normally not accessible because distributed actor protection
  let constant: NotCodable = .init()

  // normally not accessible because distributed actor protection
  func hi() -> String { "yay" }

  func justChecking() { }
  // expected-note@-1{{distributed actor-isolated instance method 'justChecking()' declared here}}
}

func test(actorSystem: FakeActorSystem) async {
  let da = DA(actorSystem: actorSystem)

  let x: _local DA = da // expected-error{{'_local' is not support in patterns}}

  // Double check normal distributed isolation still works:
  await da.justChecking()
  // expected-error@-1{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}

  // === WhenLocal
  // --- functions
  _ = await da.whenLocal { (myself: _local DA) in // can spell this
    await myself.hi() // ok! Safely by-passed distributed actor isolation
  }
  // FIXME(distributed): _local must be spelled out in the closure nowadays, remove this limitation
//  _ = await da.whenLocal { myself in
//    await myself.hi()
//  }

  // --- properties
  _ = await da.whenLocal { (myself: _local DA) in
    myself.constant // ok! Safely by-passed distributed actor isolation
  }

  // === Passing Around a known to be local distributed actor
  let closure: (_local DA) async -> String = { (da: _local DA) in
    await da.hi()
  }
  _ = await closure(da)
  // FIXME(distributed): _local must be spelled out in the closure nowadays, remove this limitation
//  let closure: (_local DA) async -> String = { da in
//    await da.hi()
//  }
}
