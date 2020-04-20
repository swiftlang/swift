// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Indirect -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Indirect -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Indirect.symbols.json

public protocol P {
  func foo()
}

public protocol Q : P {}

extension Q {
  public func foo() {}
}

// CHECK-DAG: "kind": "defaultImplementationOf",{{[[:space:]]*}}"source": "s:8Indirect1QPAAE3fooyyF",{{[[:space:]]*}}"target": "s:8Indirect1PP3fooyyF"

// Since foo is a default implementation of a requirment, we don't consider this to be a "member"
// CHECK-NOT: memberOf
