// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/differentiable_attr_silgen_other_module.swift -emit-module-path %t/differentiable_attr_silgen_other_module.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -verify -I %t -primary-file %s | %FileCheck %s -check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify -I %t -primary-file %s | %FileCheck %s -check-prefix=CHECK-SIL
import differentiable_attr_silgen_other_module

// After SILGen, a SIL `[differentiable]` attribute on a function from the
// current module should have JVP/VJP names only if the AST `@differentiable`
// attribute does.
// For external functions, `[differentiable]` attribute JVP/VJP names should
// always exist. The differentiation pass is guaranteed to fill in
// `[differentiable]` attribute JVP/VJP names.
_ = pullback(at: Wrapper(1)) { x in x + x * x }

// CHECK-SILGEN-LABEL: // static Wrapper.* infix(_:_:)
// CHECK-SILGEN-NEXT: sil [differentiable source 0 wrt 0, 1 jvp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ__jvp_src_0_wrt_0_1 vjp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ__vjp_src_0_wrt_0_1] @$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ : $@convention(method) (Wrapper, Wrapper, @thin Wrapper.Type) -> Wrapper
// CHECK-SIL-LABEL: // static Wrapper.* infix(_:_:)
// CHECK-SIL-NEXT: sil [differentiable source 0 wrt 0, 1 jvp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ__jvp_src_0_wrt_0_1 vjp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ__vjp_src_0_wrt_0_1] @$s39differentiable_attr_silgen_other_module7WrapperV1moiyA2C_ACtFZ : $@convention(method) (Wrapper, Wrapper, @thin Wrapper.Type) -> Wrapper
// CHECK-SILGEN-LABEL: // static Wrapper.+ infix(_:_:)
// CHECK-SILGEN-NEXT: sil [differentiable source 0 wrt 0, 1 vjp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1poiyA2C_ACtFZ__vjp_src_0_wrt_0_1] @$s39differentiable_attr_silgen_other_module7WrapperV1poiyA2C_ACtFZ : $@convention(method) (Wrapper, Wrapper, @thin Wrapper.Type) -> Wrapper
// CHECK-SIL-LABEL: // static Wrapper.+ infix(_:_:)
// CHECK-SIL-NEXT: sil [differentiable source 0 wrt 0, 1 jvp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1poiyA2C_ACtFZ__jvp_src_0_wrt_0_1 vjp @AD__$s39differentiable_attr_silgen_other_module7WrapperV1poiyA2C_ACtFZ__vjp_src_0_wrt_0_1] @$s39differentiable_attr_silgen_other_module7WrapperV1poiyA2C_ACtFZ : $@convention(method) (Wrapper, Wrapper, @thin Wrapper.Type) -> Wrapper
