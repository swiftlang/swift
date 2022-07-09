// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name RequirementOf -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name RequirementOf -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/RequirementOf.symbols.json

public protocol P {
  var x: Int { get }
}

// CHECK: "kind": "requirementOf"
// CHECK-NEXT: "source": "s:13RequirementOf1PP1xSivp"
// CHECK-NEXT: "target": "s:13RequirementOf1PP"
// CHECK-NOT: defaultImplementationOf
