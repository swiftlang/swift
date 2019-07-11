// RUN: %target-swift-frontend -parse-stdlib -Onone -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -Onone -emit-sil -verify -DFAIL_TEST %s

// CHECK-LABEL: sil hidden @$s20polymorphic_builtins11successTestyBi32_Bv4_Bi32_Bv4__Bi32_Bv4_tF : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "add_Vec4xInt32"([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins11successTestyBi32_Bv4_Bi32_Bv4__Bi32_Bv4_tF'
func successTest(_ x: Builtin.Vec4xInt32, _ y: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return Builtin.generic_add(x, y)
}

@_transparent
func genericSuccessTestCallee<T>(_ x : T, _ y : T) -> T {
  return Builtin.generic_add(x, y)
}

// CHECK-LABEL: sil @$s20polymorphic_builtins24genericSuccessTestCalleryBi32_Bv4_Bi32_Bv4__Bi32_Bv4_tF : $@convention(thin) (Builtin.Vec4xInt32, Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
// CHECK: bb0([[ARG0:%.*]] : $Builtin.Vec4xInt32, [[ARG1:%.*]] : $Builtin.Vec4xInt32):
// CHECK:   [[RESULT:%.*]] = builtin "add_Vec4xInt32"([[ARG0]] : $Builtin.Vec4xInt32, [[ARG1]] : $Builtin.Vec4xInt32) : $Builtin.Vec4xInt32
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s20polymorphic_builtins24genericSuccessTestCalleryBi32_Bv4_Bi32_Bv4__Bi32_Bv4_tF'
public func genericSuccessTestCaller(_ x : Builtin.Vec4xInt32, _ y : Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return genericSuccessTestCallee(x, y)
}

#if FAIL_TEST

public struct MyVal {
  var x: Builtin.Int32
}

public func callWithBadValue(_ x : MyVal, _ y: MyVal) -> MyVal {
  return Builtin.generic_add(x, y) // expected-error {{division by zero}}
}

#endif
