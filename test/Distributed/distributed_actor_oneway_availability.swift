// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -enable-experimental-feature SendCalls -I %t %t/main.swift

// Embedded Swift has no OS availability, so the same declarations built for
// the same old deployment target are not diagnosed
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -target %target-cpu-apple-macos14 %t/embedded.swift %S/Embedded/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// The synchronous thunk of a '@oneway' distributed func calls
// 'remoteCallOneway' of the '_DistributedOnewayActorSystem', so outside of
// Embedded Swift such a func requires the Swift 6.5 runtime

//--- main.swift
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  // expected-note@-1 2{{add '@available' attribute to enclosing distributed actor}}

  @oneway distributed func hello() {}
  // expected-error@-1{{'@oneway' distributed instance method 'hello()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}

  @available(SwiftStdlib 6.5, *)
  @oneway distributed func annotated() {}

  @oneway distributed func sync(_ n: Int) {}
  // expected-error@-1{{'@oneway' distributed instance method 'sync' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}
}

@available(SwiftStdlib 6.5, *)
distributed actor AnnotatedGreeter {
  @oneway distributed func hello() {}
}

extension Greeter {
  @available(SwiftStdlib 6.5, *)
  @oneway distributed func inExtension() {}
}

// A plain actor '@oneway' method has no remote call target, so it has no
// availability requirement
actor Worker {
  @oneway func work() {}
}

protocol Service: DistributedActor where ActorSystem == FakeActorSystem {
  // expected-note@-1{{add '@available' attribute to enclosing protocol}}
  @oneway distributed func notify()
  // expected-error@-1{{'@oneway' distributed instance method 'notify()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}

  @available(SwiftStdlib 6.5, *)
  @oneway distributed func annotatedNotify()

  distributed func twoWay()
}

@available(SwiftStdlib 6.5, *)
protocol AnnotatedService: DistributedActor where ActorSystem == FakeActorSystem {
  @oneway distributed func notify()
}

//--- embedded.swift
import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  @oneway distributed func hello() {}
}

protocol Service: DistributedActor where ActorSystem == EmbeddedFakeRoundtripActorSystem {
  @oneway distributed func notify()
}

actor Worker {
  @oneway func work() {}
}
