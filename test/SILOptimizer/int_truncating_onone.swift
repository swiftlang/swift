// RUN: %target-build-swift -Onone %s -module-name=test -emit-sil | %FileCheck %s

// Make sure that integer truncation does not result in a generic function call,
// even in -Onone.

// CHECK-LABEL: sil @$s4test6testitys5Int32Vs5Int64VF : $@convention(thin) (Int64) -> Int32
// CHECK-NOT: apply
// CHECK:     builtin
// CHECK-NOT: apply
// CHECK: } // end sil function '$s4test6testitys5Int32Vs5Int64VF'
public func testit(_ x: Int64) -> Int32 {
  return Int32(truncatingIfNeeded: x)
}
