// RUN: %target-swift-frontend -parse-as-library -module-name test %s -O -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: PTRSIZE=64

// CHECK-LABEL: sil @$s4test17dont_fold_inf_cmpySbSfF :
// CHECK:         builtin "fcmp_olt_FPIEEE32"
// CHECK:       } // end sil function '$s4test17dont_fold_inf_cmpySbSfF'
public func dont_fold_inf_cmp(_ f: Float) -> Bool {
  (f + 0) < .infinity
}

// CHECK-LABEL: sil @$s4test09fold_inf_C4_cmpSbyF :
// CHECK:         [[ZERO:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:         [[B:%.*]] = struct $Bool ([[ZERO]])
// CHECK:         return [[B]]
// CHECK:       } // end sil function '$s4test09fold_inf_C4_cmpSbyF'
public func fold_inf_inf_cmp() -> Bool {
  0x1.0p128 < Float.infinity
}

public struct FP32: FixedPoint {
  public var bits: Int32

  public init(bits: Bits) {
    self.bits = bits
  }
}

// CHECK-LABEL: sil @$s4test0A19FloatLiteralFoldingAA4FP32VyF :
// CHECK:         [[L:%.*]] = integer_literal $Builtin.Int32, 65536
// CHECK:         [[I:%.*]] = struct $Int32 ([[L]])
// CHECK:         [[F:%.*]] = struct $FP32 ([[I]])
// CHECK:         return [[F]]
// CHECK:       } // end sil function '$s4test0A19FloatLiteralFoldingAA4FP32VyF'
public func testFloatLiteralFolding() -> FP32 {
  return 1.0
}

public protocol FixedPoint: ExpressibleByFloatLiteral {
  associatedtype Bits: FixedWidthInteger
  var bits: Bits { get set }

  init(bits: Bits)
}

extension FixedPoint {
  public init(floatLiteral value: Double) {
    self.init(value)
  }

  init<F: BinaryFloatingPoint>(_ value: F) {
    let s = F(sign: .plus, exponent: F.Exponent(16), significand: 1)
    let r = (s * value).rounded(.toNearestOrEven)
    self.init(bits: Bits(exactly: r)!)
  }
}

