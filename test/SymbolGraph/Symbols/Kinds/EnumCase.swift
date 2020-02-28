// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Kinds -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Kinds -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Kinds.symbols.json

public enum E {
  // CHECK: "identifier": "swift.enum.case"
  // CHECK-NEXT: "displayName": "Case"
  // CHECK: pathComponents
  // CHECK-NEXT: "E"
  // CHECK-NEXT: "oneCase"
  case oneCase
}
