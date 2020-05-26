// RUN: %empty-directory(%t)

// Generate the parseable interface of the current file via the merge-modules step
// RUN: %target-build-swift -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/TestMerge.swiftinterface -module-name Test %s

// Generate the parseable interface of the current file via a single frontend invocation
// RUN: %target-swift-frontend -typecheck  -enable-objc-interop -emit-module-interface-path %t/TestSingle.swiftinterface -module-name Test %s

// Make sure both don't add override for inits shadowing convenience initializers
// RUN: %FileCheck --check-prefixes=CHECK,SINGLE %s < %t/TestSingle.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK,MERGE %s < %t/TestMerge.swiftinterface

// Check we can consume the interface without issue
// RUN: %target-swift-frontend -swift-version 5 -compile-module-from-interface -o %t/Test.swiftmodule %t/TestSingle.swiftinterface
// RUN: %target-swift-frontend -swift-version 5 -compile-module-from-interface -o %t/Test.swiftmodule %t/TestMerge.swiftinterface

public class Base {
  let x: Int
  public init(x: Int) {
    self.x = x
  }
  convenience public init() {
    self.init(x: 1)
  }
}

public class Derived: Base {
  // CHECK: {{^}}  public init(z: Swift.Int)
  public init(z: Int) {
    super.init(x: z)
  }
  // MERGE: {{^}}  public convenience init()
  // SINGLE: {{^}}  convenience public init()
  convenience public init() {
    self.init(z: 1)
  }
}

public class Derived2: Base {
  // CHECK: {{^}}  public init()
  public init() {
    super.init(x: 1)
  }

  // MERGE: {{^}}  override public convenience init(x: Swift.Int)
  // SINGLE: {{^}}  override convenience public init(x: Swift.Int)
  override convenience public init(x: Int) {
    self.init()
  }
}
