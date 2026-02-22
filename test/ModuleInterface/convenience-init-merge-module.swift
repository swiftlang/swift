// REQUIRES: legacy_swift_driver
// RUN: %empty-directory(%t)

// Generate the parseable interface of the current file via the merge-modules step
// RUN: %target-build-swift -no-emit-module-separately -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/TestMerge.swiftinterface -module-name Test %s

// Make sure both don't add override for inits shadowing convenience initializers
// RUN: %FileCheck %s < %t/TestMerge.swiftinterface

// Check we can consume the interface without issue
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
  // CHECK: {{^}}  public init(z: Swift::Int)
  public init(z: Int) {
    super.init(x: z)
  }
  // CHECK: {{^}}  public convenience init()
  convenience public init() {
    self.init(z: 1)
  }
}

public class Derived2: Base {
  // CHECK: {{^}}  public init()
  public init() {
    super.init(x: 1)
  }

  // CHECK: {{^}}  override public convenience init(x: Swift::Int)
  override convenience public init(x: Int) {
    self.init()
  }
}
