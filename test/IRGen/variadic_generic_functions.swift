// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// REQUIRES: PTRSIZE=64

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f11tyxxQp_tlF"(i8** noalias nocapture dereferenceable(8) %0, i64 %1, %swift.type* %T)
func f1<T...>(t: T...) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f21t1uyxxQp_q_q_Qptr0_lF"(i8** noalias nocapture dereferenceable(8) %0, i8** noalias nocapture dereferenceable(8) %1, i64 %2, i64 %3, %swift.type* %T, %swift.type* %U)
func f2<T..., U...>(t: T..., u: U...) {}

// CHECK-LABEL: define hidden swiftcc void @"$s26variadic_generic_functions2f31t1uyxxQp_q_q_Qptq_Rhzr0_lF"(i8** noalias nocapture dereferenceable(8) %0, i8** noalias nocapture dereferenceable(8) %1, i64 %2, %swift.type* %T, %swift.type* %U)
func f3<T..., U...>(t: T..., u: U...) where ((T, U)...): Any {}
