// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %t/main.swift

// Embedded Swift has no OS availability, so the same declarations built for
// the same old deployment target are not diagnosed
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -target %target-cpu-apple-macos14 %t/embedded.swift %S/Embedded/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: OS=macosx

// A pre-6.5 recipient cannot demangle the remote call target identifier of a
// 'oneway' distributed func, so outside of Embedded Swift such a func requires
// the Swift 6.5 runtime

//--- main.swift
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  // expected-note@-1 2{{add '@available' attribute to enclosing distributed actor}}

  distributed func hello() oneway {}
  // expected-error@-1{{'oneway' distributed instance method 'hello()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}

  // The two-way overload next to it has no such requirement
  distributed func hello() {}

  @available(SwiftStdlib 6.5, *)
  distributed func annotated() oneway {}

  distributed func sync(_ n: Int) oneway {}
  // expected-error@-1{{'oneway' distributed instance method 'sync' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}
}

@available(SwiftStdlib 6.5, *)
distributed actor AnnotatedGreeter {
  distributed func hello() oneway {}
}

extension Greeter {
  @available(SwiftStdlib 6.5, *)
  distributed func inExtension() oneway {}
}

// A plain actor 'oneway' method has no remote call target, so it has no
// availability requirement
actor Worker {
  func work() oneway {}
}

protocol Service: DistributedActor where ActorSystem == FakeActorSystem {
  // expected-note@-1{{add '@available' attribute to enclosing protocol}}
  distributed func notify() oneway
  // expected-error@-1{{'oneway' distributed instance method 'notify()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}

  @available(SwiftStdlib 6.5, *)
  distributed func annotatedNotify() oneway

  distributed func twoWay()
}

@available(SwiftStdlib 6.5, *)
protocol AnnotatedService: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func notify() oneway
}

//--- embedded.swift
import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func hello() oneway {}
  distributed func hello() {}
}

protocol Service: DistributedActor where ActorSystem == EmbeddedFakeRoundtripActorSystem {
  distributed func notify() oneway
}

actor Worker {
  func work() oneway {}
}
