// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InheritsFrom -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InheritsFrom -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InheritsFrom.symbols.json

public class Base {}
public class Derived: Base {}

// CHECK: "kind": "inheritsFrom"
// CHECK-NEXT: "source": "s:12InheritsFrom7DerivedC"
// CHECK-NEXT: "target": "s:12InheritsFrom4BaseC"
