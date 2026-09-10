// RUN: %target-swift-frontend -module-name test -O -emit-sil %s | %FileCheck %s

// rdar://187051148: When the body of a loop contains a call to a callee whose
// definition is unknown, the unroller cannot estimate the cost of unrolling and
// used to over-unroll, producing an enormous number of basic blocks.

// CHECK-LABEL: sil @$s4test7viaSIMDys6SIMD16Vys5UInt8VGADys5Int32VGF :
// CHECK-NOT: bb{{[0-9][0-9][0-9]}}
public func viaSIMD(_ v: SIMD16<Int32>) -> SIMD16<UInt8> {
  SIMD16(clamping: v)
}
