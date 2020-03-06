// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DefaultImplementation -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DefaultImplementation -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DefaultImplementation.symbols.json

public protocol P {
  func foo()
  func bar()
  func baz()
}

extension P {
  public func foo() {}
}

public extension P {
  func bar() {}
}

public struct Outer: P {
  // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3fooyyF::SYNTHESIZED::s:21DefaultImplementation5OuterV",
  // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3baryyF::SYNTHESIZED::s:21DefaultImplementation5OuterV",
  // CHECK-DAG: "precise": "s:21DefaultImplementation5OuterV3bazyyF",
  public func baz() {}
}


extension Outer {
  public struct Inner: P {
    // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3fooyyF::SYNTHESIZED::s:21DefaultImplementation5OuterV5InnerV",
    // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3baryyF::SYNTHESIZED::s:21DefaultImplementation5OuterV5InnerV",
    // CHECK-DAG: "precise": "s:21DefaultImplementation5OuterV5InnerV3bazyyF"
    public func baz() {}
  }
}

extension Outer {
  public struct FromExtension: P {
    // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3fooyyF::SYNTHESIZED::s:21DefaultImplementation5OuterV13FromExtensionV"
    // CHECK-DAG: "precise": "s:21DefaultImplementation1PPAAE3baryyF::SYNTHESIZED::s:21DefaultImplementation5OuterV13FromExtensionV"
    // CHECK-DAG: "precise": "s:21DefaultImplementation5OuterV13FromExtensionV3bazyyF"
    public func baz() {}
  }
}

// CHECK-NOT: {{.*baz}}::SYNTHESIZED::
