// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s -check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s -check-prefix=CHECK-SIL

// After SILGen, SIL `[differentiable]` should have jvp/vjp names only if the AST `@differentiable` attribute does.
// The differentiation pass is guaranteed to fill in jvp/vjp names.

_ = gradient(at: Float(1)) { x in x + x * x }

// CHECK-SILGEN-LABEL: // static Float.* infix(_:_:)
// CHECK-SILGEN-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1] @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL-LABEL: // static Float.* infix(_:_:)
// CHECK-SIL-NEXT: sil public_external [transparent] [serialized] [differentiable source 0 wrt 0, 1 jvp @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @AD__$sSf1moiyS2f_SftFZ__vjp_src_0_wrt_0_1] [differentiable source 0 wrt 0, 1 jvp @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @AD__$sSf1moiyS2f_SftFZ__vjp_src_0_wrt_0_1] @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float

// CHECK-SILGEN-LABEL: // static Float.+ infix(_:_:)
// CHECK-SILGEN-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1] @$sSf1poiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL-LABEL: // static Float.+ infix(_:_:)
// CHECK-SIL-NEXT: sil public_external [transparent] [serialized] [differentiable source 0 wrt 0, 1 jvp @AD__$sSf1poiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @AD__$sSf1poiyS2f_SftFZ__vjp_src_0_wrt_0_1] [differentiable source 0 wrt 0, 1 jvp @AD__$sSf1poiyS2f_SftFZ__jvp_src_0_wrt_0_1 vjp @AD__$sSf1poiyS2f_SftFZ__vjp_src_0_wrt_0_1] @$sSf1poiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
