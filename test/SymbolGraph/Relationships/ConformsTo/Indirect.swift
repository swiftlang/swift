// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Indirect -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Indirect -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Indirect.symbols.json

public protocol P {
  func foo()
}

public protocol Q : P {}

public struct S : Q {
  public func foo() {}
}

// Q : P
// CHECK-DAG: "kind": "conformsTo",{{[[:space:]]*}}"source": "s:8Indirect1QP",{{[[:space:]]*}}"target": "s:8Indirect1PP"

// S : P
// CHECK-DAG: "kind": "conformsTo",{{[[:space:]]*}}"source": "s:8Indirect1SV",{{[[:space:]]*}}"target": "s:8Indirect1PP"

// S : Q
// CHECK-DAG: "kind": "conformsTo",{{[[:space:]]*}}"source": "s:8Indirect1SV",{{[[:space:]]*}}"target": "s:8Indirect1QP"
