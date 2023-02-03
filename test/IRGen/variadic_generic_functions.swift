// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// REQUIRES: PTRSIZE=64

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f11tyxxQp_tlF"(%swift.opaque** noalias nocapture %0, i64 %1, %swift.type** %T)
func f1<T...>(t: repeat each T) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f21t1uyxxQp_q_q_Qptr0_lF"(%swift.opaque** noalias nocapture %0, %swift.opaque** noalias nocapture %1, i64 %2, i64 %3, %swift.type** %T, %swift.type** %U)
func f2<T..., U...>(t: repeat each T, u: repeat each U) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f31t1uyxxQp_q_xQptq_Rhzr0_lF"(%swift.opaque** noalias nocapture %0, %swift.opaque** noalias nocapture %1, i64 %2, %swift.type** %T, %swift.type** %U)
func f3<T..., U...>(t: repeat each T, u: repeat each U) where (repeat (each T, each U)): Any {}
