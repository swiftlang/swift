// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -swift-version 5  -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s
import Distributed

@available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) // SwiftStdlib 5.7
public typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

// CHECK: {{.*}}distributed {{.*}} actor CheckMe
@available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) // SwiftStdlib 5.7
public distributed actor CheckMe {
  distributed func test() {
    // ...
  }

}

// CHECK: public struct HasDistributedActors
@available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) // SwiftStdlib 5.7
public struct HasDistributedActors {
  let check: CheckMe

  func test() async throws {
    try await check.test()
  }
}