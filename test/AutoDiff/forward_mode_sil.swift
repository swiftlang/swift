// RUN: %target-swift-frontend -emit-sil -Xllvm -run-jvp-generation  -verify %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -sil-print-after=differentiation -Xllvm -run-jvp-generation -o /dev/null 2>&1 %s | %FileCheck %s -check-prefix=CHECK-SIL


//===----------------------------------------------------------------------===//
// Unary
//===----------------------------------------------------------------------===//

@differentiable
@_silgen_name("unary")
func unary(_ x: Float) -> Float {
  return x * x * x
}
// CHECK-DATA-STRUCTURES: struct _AD__unary_bb0__DF__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var differential_0: (Float, Float) -> Float { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var differential_1: (Float, Float) -> Float { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__unary_bb0__Succ__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }


// CHECK-SIL-LABEL: sil hidden @AD__unary__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK-SIL: // %0
// CHECK-SIL: bb0(%0 : $Float):
// CHECK-SIL:   debug_value %0 : $Float, let, name "x", argno 1 // id: %1
// CHECK-SIL:   %2 = metatype $@thin Float.Type                 // user: %18
// CHECK-SIL:   %3 = metatype $@thin Float.Type                 // user: %9
// CHECK-SIL:   %4 = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float // users: %8, %5
// CHECK-SIL:   retain_value %4 : $@convention(method) (Float, Float, @thin Float.Type) -> Float // id: %5
// CHECK-SIL:   // function_ref static Float._vjpMultiply(lhs:rhs:)
// CHECK-SIL:   %6 = function_ref @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) // users: %9, %8
// CHECK-SIL:   // function_ref static Float._vjpMultiply(lhs:rhs:)
// CHECK-SIL:   %7 = function_ref @$sSf12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) // user: %8
// CHECK-SIL:   %8 = autodiff_function [wrt 0 1] [order 1] %4 : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {%6 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), %7 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))} // user: %10
// CHECK-SIL:   %9 = apply %6(%0, %0, %3) : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) // users: %12, %11
// CHECK-SIL:   release_value %8 : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float // id: %10
// CHECK-SIL:   %11 = tuple_extract %9 : $(Float, @callee_guaranteed (Float, Float) -> Float), 0 // user: %18
// CHECK-SIL:   %12 = tuple_extract %9 : $(Float, @callee_guaranteed (Float, Float) -> Float), 1 // user: %22
// CHECK-SIL:   // function_ref static Float.* infix(_:_:)
// CHECK-SIL:   %13 = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float // users: %17, %14
// CHECK-SIL:   retain_value %13 : $@convention(method) (Float, Float, @thin Float.Type) -> Float // id: %14
// CHECK-SIL:   // function_ref AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1
// CHECK-SIL:   %15 = function_ref @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) // users: %18, %17
// CHECK-SIL:   // function_ref static Float._vjpMultiply(lhs:rhs:)
// CHECK-SIL:   %16 = function_ref @$sSf12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) // user: %17
// CHECK-SIL:   %17 = autodiff_function [wrt 0 1] [order 1] %13 : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {%15 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), %16 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))} // user: %19
// CHECK-SIL:   %18 = apply %15(%11, %0, %2) : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) // users: %21, %20
// CHECK-SIL:   release_value %17 : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float // id: %19
// CHECK-SIL:   %20 = tuple_extract %18 : $(Float, @callee_guaranteed (Float, Float) -> Float), 0 // user: %25
// CHECK-SIL:   %21 = tuple_extract %18 : $(Float, @callee_guaranteed (Float, Float) -> Float), 1 // user: %22
// CHECK-SIL:   %22 = struct $_AD__unary_bb0__DF__src_0_wrt_0 (%12 : $@callee_guaranteed (Float, Float) -> Float, %21 : $@callee_guaranteed (Float, Float) -> Float) // user: %24
// CHECK-SIL:   // function_ref AD__unary__differential_src_0_wrt_0
// CHECK-SIL:   %23 = function_ref @AD__unary__differential_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed _AD__unary_bb0__DF__src_0_wrt_0) -> Float // user: %24
// CHECK-SIL:   %24 = partial_apply [callee_guaranteed] %23(%22) : $@convention(thin) (Float, @guaranteed _AD__unary_bb0__DF__src_0_wrt_0) -> Float // user: %25
// CHECK-SIL:   %25 = tuple (%20 : $Float, %24 : $@callee_guaranteed (Float) -> Float) // user: %26
// CHECK-SIL:   return %25 : $(Float, @callee_guaranteed (Float) -> Float) // id: %26
// CHECK-SIL: } // end sil function 'AD__unary__jvp_src_0_wrt_0'
