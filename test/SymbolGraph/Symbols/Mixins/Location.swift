// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Location -emit-module-path %t/Location.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Location -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Location.symbols.json "-DTESTFILE=%s"

/// This is a struct.
public struct MyStruct {
  public var x = 1 
}

// CHECK: location
// CHECK-NEXT: uri
// CHECK-NEXT: position
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 14 

// CHECK: location
// CHECK-NEXT: uri
// CHECK-NEXT: position
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 13
