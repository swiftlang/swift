// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil @$S20string_interpolation11interpolate_1uSSx_q_ts23CustomStringConvertibleR_r0_lF : $@convention(thin) <T, U where U : CustomStringConvertible> (@in_guaranteed T, @in_guaranteed U) -> @owned String
// CHECK: function_ref @$SSS26stringInterpolationSegmentSSx_tcs23CustomStringConvertibleRzlufC
// CHECK: function_ref @$SSS26stringInterpolationSegmentSSx_tclufC
// CHECK: function_ref @$SSS26stringInterpolationSegmentSSx_tcs23CustomStringConvertibleRzlufC
// CHECK: function_ref @$SSS26stringInterpolationSegmentSSx_tcs23CustomStringConvertibleRzlufC
// CHECK: function_ref @$SSS26stringInterpolationSegmentSSx_tcs23CustomStringConvertibleRzlufC
// CHECK: return
public func interpolate<T, U : CustomStringConvertible>(_ t: T, u: U) -> String {
  return "a\(t)b\(u)c"
}
