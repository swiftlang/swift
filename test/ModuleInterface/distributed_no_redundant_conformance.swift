// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface

import Distributed

// Note that tat while an actor is implicitly conforming to Actor, we don't need to print this in interfaces
// as it would cause 'redundant conformance of ... to (Distributed)Actor' issues.

public actor Example {}

// CHECK-NOT: extension TestResilient.Example : _Concurrency.Actor {}

@available(SwiftStdlib 5.7, *)
public distributed actor DistributedExample {
  public typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func example() {}
}

// CHECK: distributed public actor DistributedExample {

// CHECK-NOT: extension TestResilient.DistributedExample : Distributed.DistributedActor {}
