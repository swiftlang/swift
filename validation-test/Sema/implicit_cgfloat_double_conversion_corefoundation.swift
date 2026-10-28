// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-swift-emit-silgen -swift-version 5 %s | %FileCheck %s

// REQUIRES: objc_interop

// On Darwin, CGFloat is declared by the CoreFoundation overlay, so importing
// CoreFoundation on its own is enough to form the implicit conversion.

import CoreFoundation

// CHECK-LABEL: sil hidden [ossa] @{{.*}}19testCGFloatToDouble{{.*}} : $@convention(thin) (CGFloat) -> Double {
// CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC :
func testCGFloatToDouble(_ x: CGFloat) -> Double {
  return x // Ok
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}19testDoubleToCGFloat{{.*}} : $@convention(thin) (Double) -> CGFloat {
// CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC :
func testDoubleToCGFloat(_ x: Double) -> CGFloat {
  return x // Ok
}

// CHECK-LABEL: sil hidden [ossa] @$s49implicit_cgfloat_double_conversion_corefoundation24testContextualConversionyyF : $@convention(thin) () -> () {
func testContextualConversion() {
  let y: CGFloat = 0.0
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC :
  let _: Double = y // Ok
}
