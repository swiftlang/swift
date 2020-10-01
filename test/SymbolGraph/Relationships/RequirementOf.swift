// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConformsTo -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConformsTo -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ConformsTo.symbols.json

public protocol P {
  var x: Int { get }
}

// CHECK: "kind": "requirementOf"
// CHECK-NEXT: "source": "s:10ConformsTo1PP1xSivp"
// CHECK-NEXT: "target": "s:10ConformsTo1PP"
// CHECK-NOT: defaultImplementationOf
