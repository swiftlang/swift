// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-ir -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -primary-file %s

// REQUIRES: PTRSIZE=64

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f11tyxxQp_tRvzlF"(%swift.opaque** noalias nocapture %0, i64 %1, %swift.type** %"each T")
func f1<each T>(t: repeat each T) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f21t1uyxxQp_q_q_QptRvzRv_r0_lF"(%swift.opaque** noalias nocapture %0, %swift.opaque** noalias nocapture %1, i64 %2, i64 %3, %swift.type** %"each T", %swift.type** %"each U")
func f2<each T, each U>(t: repeat each T, u: repeat each U) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f31t1uyxxQp_q_xQptRvzRv_q_Rhzr0_lF"(%swift.opaque** noalias nocapture %0, %swift.opaque** noalias nocapture %1, i64 %2, %swift.type** %"each T", %swift.type** %"each U")
func f3<each T, each U>(t: repeat each T, u: repeat each U) where (repeat (each T, each U)): Any {}

protocol P {}

// CHECK-LABEL: define {{.*}}void @f4(%swift.opaque** noalias nocapture %0, i64 %1, %swift.type** %"each T", i8*** %"each T.P")
@_silgen_name("f4")
func f4<each T : P>(t: repeat each T) {}
