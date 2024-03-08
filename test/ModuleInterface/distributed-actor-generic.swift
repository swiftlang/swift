// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: distributed

import Distributed

@available(SwiftStdlib 6.0, *)
public distributed actor DAG<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable> {
}
