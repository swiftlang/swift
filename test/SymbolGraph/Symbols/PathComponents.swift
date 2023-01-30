// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PathComponents -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PathComponents -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/PathComponents.symbols.json
// RUN: %FileCheck %s --input-file %t/PathComponents@Swift.symbols.json --check-prefix EXTENSION
// RUN: %FileCheck %s --input-file %t/PathComponents@Swift.symbols.json --check-prefix EXTENSION_EBS

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


public extension String {
  public struct Inner {
    public var x: Int { 1 }
  }
}

// EXTENSION:         "precise": "s:SS14PathComponentsE5InnerV1xSivp"
// EXTENSION-NEXT:    "interfaceLanguage": "swift"
// EXTENSION-NEXT:  },
// EXTENSION-NEXT:  "pathComponents": [
// EXTENSION-NEXT:   "String"
// EXTENSION-NEXT:   "Inner"
// EXTENSION-NEXT:   "x"
// EXTENSION-NEXT: ]

// EXTENSION_EBS:         "precise": "s:e:s:SS14PathComponentsE5InnerV"
// EXTENSION_EBS-NEXT:    "interfaceLanguage": "swift"
// EXTENSION_EBS-NEXT:  },
// EXTENSION_EBS-NEXT:  "pathComponents": [
// EXTENSION_EBS-NEXT:   "String"
// EXTENSION_EBS-NEXT: ]
