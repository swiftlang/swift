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
  private var x: Int

  // CHECK: public convenience init(convenience x: Swift.Int)
  public init(convenience x: Int) {
    self.init(designated: x)
  }

  // CHECK: public init()
  public init() {
    self.x = 0
  }

  // CHECK: public init(designated x: Swift.Int)
  public init(designated x: Int) {
    self.x = x
  }
}

// CHECK-LABEL: extension Library.TestActor {
@available(SwiftStdlib 5.5, *)
extension TestActor {
  // CHECK: public convenience init(convenienceInExtension x: Swift.Int)
  public init(convenienceInExtension x: Int) {
    self.init(designated: x)
  }
}
