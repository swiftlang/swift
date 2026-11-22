// RUN: %target-swift-frontend  %s -O -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// FIXME: With the once functions adopted in the stdlib, Dictionary's
// subscript(_:default:) leaves a heap reabstraction thunk that captures an
// at-most-once closure (rdar://192503315).
// XFAIL: *

public struct S {
    var x: Int

    @inline(never)
    mutating func doSomething() { }
}

// Check that all partial_applys can be optimized away so that no closure context needs to be allocated.

// CHECK-LABEL: sil @$s4test6testit_1xySDySiAA1SVGz_SitF :
// CHECK-NOT:     partial_apply
// CHECK:       } // end sil function '$s4test6testit_1xySDySiAA1SVGz_SitF'
public func testit(_ data: inout [Int: S], x: Int) {
  data[x, default: S(x: x)].doSomething()
}
