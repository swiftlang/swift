// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s
// SIL `[differentiable]` should have jvp/vjp names only if the AST `@differentiable` attribute does.

_ = gradient(at: Float(1)) { x in x + x * x }

// CHECK-LABEL: // static Float.* infix(_:_:)
// CHECK-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1 vjp @$sSf12_vjpMultiply3lhs3rhsSf_Sf_SftSfctSf_SftFZ] @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float

// CHECK-LABEL: // static Float.+ infix(_:_:)
// CHECK-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0, 1 vjp @$sSf7_vjpAdd3lhs3rhsSf_Sf_SftSfctSf_SftFZ] @$sSf1poiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
