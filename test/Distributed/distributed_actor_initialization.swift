// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ----------------------------------------------------------------------------------------------------------------

distributed actor OK0 { }

distributed actor OK1 {
  var x: Int = 1
  // ok, since all fields are initialized, the constructor can be synthesized
}

// TODO(distributed): test all the FIXITs in this file

distributed actor Bad1 {
  init() {
    // expected-error@-1 {{designated distributed actor initializer 'init()' is missing required DistributedActorSystem parameter}}
  }
}

distributed actor Bad12 {
  init(x: String) {
    // expected-error@-1 {{designated distributed actor initializer 'init(x:)' is missing required DistributedActorSystem parameter}}
  }
}

distributed actor OK2 {
  var x: Int

  init(x: Int, system: FakeActorSystem) { // ok
    self.x = x
  }
}

distributed actor Bad2 {
  var x: Int = 1

  init(system: FakeActorSystem, too many: FakeActorSystem) {
    // expected-error@-1{{designated distributed actor initializer 'init(system:too:)' must accept exactly one DistributedActorSystem parameter, found 2}}
  }
}

distributed actor OK3 {
  var x: Int

  init(y: Int, system: FakeActorSystem) {
    self.x = y
  }
}

distributed actor OKMulti {

  convenience init(y: Int, system: FakeActorSystem) { // ok
    self.init(system: system)
  }

}

distributed actor OKMultiDefaultValues {

  convenience init(y: Int, system: FakeActorSystem, x: Int = 1234) { // ok
    self.init(system: system)
  }

}

