// RUN: %target-swift-frontend -O -module-name=test -emit-sil %s | %FileCheck %s

func convert<
   T: BinaryFloatingPoint, U: BinaryFloatingPoint
 >(_ value: T, to: U.Type) -> U {
   U(value)
}

// Check that the follwing functions can be optimized to no-ops.

// CHECK-LABEL: sil @$s4test0A6DoubleyS2dF
// CHECK:   return %0
// CHECK: } // end sil function '$s4test0A6DoubleyS2dF'
public func testDouble(_ x: Double) -> Double {
  return convert(x, to: Double.self)
}

// CHECK-LABEL: sil @$s4test0A5FloatyS2fF
// CHECK:   return %0
// CHECK: } // end sil function '$s4test0A5FloatyS2fF'
public func testFloat(_ x: Float) -> Float {
  return convert(x, to: Float.self)
}


