// RUN: %target-swift-frontend -O -module-name=test -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

func convert<
  T: BinaryFloatingPoint, U: BinaryFloatingPoint
>(_ value: T, to: U.Type) -> U {
  U(value)
}

// Check that the following functions can be optimized to concrete conversions.

// CHECK-LABEL: sil @$s4test0A13DoubleToFloatySfSdF
// CHECK:      bb0(%0 : $Double):
// CHECK:        struct_extract %0 : $Double, #Double._value
// CHECK-NEXT:   builtin "fptrunc_FPIEEE64_FPIEEE32"
// CHECK-NEXT:   struct $Float
// CHECK-NEXT:   return
// CHECK-NEXT: } // end sil function '$s4test0A13DoubleToFloatySfSdF'
public func testDoubleToFloat(_ x: Double) -> Float {
  return convert(x, to: Float.self)
}

// CHECK-LABEL: sil @$s4test0A13FloatToDoubleySdSfF
// CHECK:      bb0(%0 : $Float):
// CHECK:        struct_extract %0 : $Float, #Float._value
// CHECK-NEXT:   builtin "fpext_FPIEEE32_FPIEEE64"
// CHECK-NEXT:   struct $Double
// CHECK-NEXT:   return
// CHECK-NEXT: } // end sil function '$s4test0A13FloatToDoubleySdSfF'
public func testFloatToDouble(_ x: Float) -> Double {
  return convert(x, to: Double.self)
}

// Check that the following functions can be optimized to no-ops.

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
