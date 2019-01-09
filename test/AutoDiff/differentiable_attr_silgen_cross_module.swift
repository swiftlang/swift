// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s

_ = gradient(at: Float(1)) { x in x + x * x }

// CHECK-LABEL: // static Float.* infix(_:_:)
// CHECK-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1 primitive jvp @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @$sSf12_vjpMultiply3lhs3rhsSf_Sf_SftSfctSf_SftFZ] @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float

// CHECK-LABEL: // static Float.+ infix(_:_:)
// CHECK-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1 primitive jvp @AD__$sSf1poiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @$sSf7_vjpAdd3lhs3rhsSf_Sf_SftSfctSf_SftFZ] @$sSf1poiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
