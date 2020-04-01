// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Overrides -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Overrides -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Overrides.symbols.json

public class Base {
  public var x: Int {
    return 1
  }
}

public class Derived: Base {
  public override var x: Int {
    return 2
  }
}

// CHECK: "kind": "overrides"
// CHECK-NEXT: "source": "s:9Overrides7DerivedC1xSivp"
// CHECK-NEXT: "target": "s:9Overrides4BaseC1xSivp"
