// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Library.swiftinterface

// Re-verify with -swift-version 6
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -swift-version 6 -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Library.swiftinterface

// CHECK-LABEL: public actor TestActor {
@available(SwiftStdlib 5.5, *)
public actor TestActor {
  // CHECK: public init(convenience: Swift.Int)
  public init(convenience: Int) {
    self.init()
  }
  // CHECK: public init()
  public init() {}
}
