// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift

// Feature enabled:
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix available- -target %target-swift-6.2-abi-triple -enable-experimental-feature DistributedRemoteCallSemantics -I %t 2>&1 %s

// Feature disabled:
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix disabled- -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t 2>&1 %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DistributedRemoteCallSemantics

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter { // expected-available-note 2 {{add '@available' attribute to enclosing distributed actor}}
  // expected-available-error@+2{{remote call semantics are only available in}}
  // expected-disabled-error@+1{{'remoteCall(oneway)' attribute is only valid when experimental feature DistributedRemoteCallSemantics is enabled}}
  @remoteCall(oneway)
  distributed func notify() {} // expected-available-note{{add '@available' attribute to enclosing distributed instance method}}

  // expected-available-error@+2{{remote call semantics are only available in}}
  // expected-disabled-error@+1{{'remoteCall(blocking)' attribute is only valid when experimental feature DistributedRemoteCallSemantics is enabled}}
  @remoteCall(blocking)
  distributed func fetch() -> Int { 0 } // expected-available-note{{add '@available' attribute to enclosing distributed instance method}}

  // expected-disabled-error@+2{{'remoteCall(oneway)' attribute is only valid when experimental feature DistributedRemoteCallSemantics is enabled}}
  @available(SwiftStdlib 6.5, *)
  @remoteCall(oneway)
  distributed func guardedNotify() {}

  // expected-disabled-error@+2{{'remoteCall(blocking)' attribute is only valid when experimental feature DistributedRemoteCallSemantics is enabled}}
  @available(SwiftStdlib 6.5, *)
  @remoteCall(blocking)
  distributed func guardedFetch() -> Int { 0 }
}

// A distributed func without any '@remoteCall' semantics does not need the 6.5
// runtime and is unaffected by the feature flag.
distributed actor PlainGreeter {
  distributed func plain() {}
}
