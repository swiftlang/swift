// RUN: %target-swift-frontend  %s -O -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

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
