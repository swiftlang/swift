// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TargetFallback -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TargetFallback -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TargetFallback.symbols.json

public struct S: CustomStringConvertible {
  public var x: Int
  public var description: String {
    return x.description
  }
}

// CHECK:      "kind": "conformsTo",
// CHECK-NEXT: "source": "s:14TargetFallback1SV",
// CHECK-NEXT: "target": "s:s23CustomStringConvertibleP",
// CHECK-NEXT: "targetFallback": "Swift.CustomStringConvertible"
