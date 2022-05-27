// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SymbolGraphModule -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SymbolGraphModule -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SymbolGraphModule.symbols.json --check-prefix=CHECK --check-prefix=CHECK-%target-os
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
// CHECK-macosx-NEXT:  minimumVersion
// CHECK-ios-NEXT:     minimumVersion
// CHECK-tvos-NEXT:    minimumVersion
// CHECK-watchos-NEXT: minimumVersion
// CHECK-macosx-NEXT:    major
// CHECK-ios-NEXT:       major
// CHECK-tvos-NEXT:      major
// CHECK-watchos-NEXT:   major
// CHECK-macosx-NEXT:    minor
// CHECK-ios-NEXT:       minor
// CHECK-tvos-NEXT:      minor
// CHECK-watchos-NEXT:   minor
