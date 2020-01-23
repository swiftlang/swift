// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConformsTo -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConformsTo -I %t -pretty-print -o %t/ConformsTo.symbols.json
// RUN: %FileCheck %s --input-file %t/ConformsTo.symbols.json

public protocol P {
  var x: Int { get }
}

public struct S: P {
  public var x: Int
}

// CHECK: "kind": "conformsTo"
// CHECK-NEXT: "source": "s:10ConformsTo1SV"
// CHECK-NEXT: "target": "s:10ConformsTo1PP"
