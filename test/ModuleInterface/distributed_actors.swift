// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s

import Distributed

@available(macOS 13.0, *)
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@available(macOS 13.0, *)
distributed actor CheckMe {
  distributed func test() {
    // ...
  }

}

// CHECK: public struct HasDistributedActors {
@available(macOS 13.0, *)
public struct HasDistributedActors {
  let check: CheckMe

  func test() async throws {
    try await check.test()
  }
}
