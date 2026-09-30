// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: OS=macosx
//
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.0-abi-triple -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -plugin-path %swift-plugin-dir -parse-as-library -I %t %s -dump-macro-expansions 2>&1 | %FileCheck %s

// The '@Resolvable' stubs of 'oneway' requirements carry the availability of
// the requirement, or of the protocol, so the expansion is not diagnosed.
// An unannotated requirement is diagnosed once, on the requirement

import Distributed
import FakeDistributedActorSystems

@Resolvable
protocol Annotated: DistributedActor where ActorSystem == FakeActorSystem {
  @available(SwiftStdlib 6.5, *)
  distributed func notify() oneway
  distributed func twoWay()
}

// CHECK: extension Annotated where Self: Distributed._DistributedActorStub
// CHECK: @available(SwiftStdlib 6.5, *)
// CHECK-NEXT: distributed func notify() oneway

@Resolvable
@available(SwiftStdlib 6.5, *)
protocol AnnotatedProtocol: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func notify() oneway
}

// CHECK: @__swiftmacro_{{.*}}AnnotatedProtocol10ResolvablefMe_.swift
// CHECK: @available(SwiftStdlib 6.5, *)
// CHECK-NEXT: extension AnnotatedProtocol where Self: Distributed._DistributedActorStub

@Resolvable
protocol Unannotated: DistributedActor where ActorSystem == FakeActorSystem {
  // expected-note@-1{{add '@available' attribute to enclosing protocol}}
  distributed func notify() oneway
  // expected-error@-1{{'oneway' distributed instance method 'notify()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}
}
