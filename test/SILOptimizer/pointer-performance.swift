// RUN: %target-swift-frontend %s -module-name=test -parse-as-library -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public struct S {
  var a: Int
  var b: Int
}

// Check that even with -Onone, no keypath is created.

// CHECK-LABEL: sil @$s4test6testitySPySiGSgSPyAA1SVGF :
// CHECK-NOT:     keypath
// CHECK:       } // end sil function '$s4test6testitySPySiGSgSPyAA1SVGF'
public func testit(_ p: UnsafePointer<S>) -> UnsafePointer<Int>? {
  return p.pointer(to: \.b)
}
