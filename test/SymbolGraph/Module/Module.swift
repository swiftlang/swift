// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SymbolGraphModule -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SymbolGraphModule -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SymbolGraphModule.symbols.json

public struct S {
  public var x: Int
}

// CHECK: module
// CHECK-NEXT: "name": "SymbolGraphModule"
// CHECK-NEXT: platform
// CHECK-NEXT:   architecture
// CHECK:        vendor
// CHECK-NEXT:   operatingSystem
// CHECK-NEXT:     name
// CHECK-NEXT:     minimumVersion 
// CHECK-NEXT:       major
// CHECK-NEXT:       minor
// CHECK-NEXT:       patch
