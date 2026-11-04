// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/IdempotentLib.swiftinterface) %s -module-name IdempotentLib -target %target-swift-5.7-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/IdempotentLib.swiftinterface) -module-name IdempotentLib -target %target-swift-5.7-abi-triple
// RUN: %FileCheck %s < %t/IdempotentLib.swiftinterface

// REQUIRES: concurrency
// REQUIRES: distributed

// '@_remoteCallIdempotent' only marks the targets of remote calls, so it is
// dropped for compilers which don't know it

import Distributed

// CHECK: {{.*}}distributed {{.*}}actor Worker
public distributed actor Worker {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK: #if compiler(>=5.3) && $RemoteCallIdempotent
  // CHECK-NEXT: @_remoteCallIdempotent {{.*}}func put(_ value: Swift::Int)
  // CHECK-NEXT: #else
  // CHECK-NEXT: {{^ *}}{{[a-z ]*}}func put(_ value: Swift::Int)
  // CHECK-NEXT: #endif
  @_remoteCallIdempotent
  public distributed func put(_ value: Int) {}

  // CHECK-NOT: $RemoteCallIdempotent
  // CHECK: func get() -> Swift::Int
  public distributed func get() -> Int { 0 }
}
