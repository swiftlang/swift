// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s --dump-input=always < %t/TestResilient.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s

import Distributed

// Note that tat while an actor is implicitly conforming to Actor, we don't need to print this in interfaces
// as it would cause 'redundant conformance of ... to (Distributed)Actor' issues.

public actor Example {}

// CHECK-NOT: extension TestResilient.Example : _Concurrency.Actor {}

public distributed actor DistributedExample {
  public typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func example() {}
}

// CHECK: distributed public actor DistributedExample {

// CHECK-NOT: extension TestResilient.DistributedExample : Distributed.DistributedActor {}