// RUN: %target-swift-frontend  %s -O -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public protocol P {
  func takePointer<T>(_ p: UnsafePointer<T>)
}

extension P {
  @inline(__always)
  func genericFunc<T>(_ x: inout T) {
    withUnsafePointer(to: &x) {
      takePointer($0)
    }
  }
}

// CHECK-LABEL: sil {{.*}}@$s4test6testityyAA1P_pF
// CHECK-NOT:     function_ref
// CHECK:         witness_method
// CHECK:       } // end sil function '$s4test6testityyAA1P_pF' 
public func testit(_ p: P) {
  var x = 0
  p.genericFunc(&x)
}

