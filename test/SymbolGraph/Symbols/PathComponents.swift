// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PathComponents -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PathComponents -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PathComponents.symbols.json

public struct Outer {
  public struct Inner {
    public var x = 1
  }
}

// CHECK:         "precise": "s:14PathComponents5OuterV5InnerV1xSivp"
// CHECK-NEXT:    "interfaceLanguage": "swift"
// CHECK-NEXT:  },
// CHECK-NEXT:  "pathComponents": [
// CHECK-NEXT:   "Outer"
// CHECK-NEXT:   "Inner"
// CHECK-NEXT:   "x"
// CHECK-NEXT: ]
