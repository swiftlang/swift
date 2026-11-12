// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_function_conversion %S/../../validation-test/closure_specialization/adcs_function_conversion.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=DIRECT < %t.passes
// RUN: %FileCheck %s --check-prefix=GENERIC < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// Compile the exact Swift runtime regression. Dropping @Sendable is an
// ABI-neutral conversion that survives until ADCS, including when followed
// by a subset-parameters thunk.

// INPUT-LABEL: // reverse-mode derivative of branchConverted(_:_:_:)
// INPUT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} : $@convention(thin) @Sendable
// INPUT-NEXT: %[[CONVERTED:[0-9]+]] = convert_function %[[ROOT]] to $@callee_guaranteed (Float) -> Float
// INPUT-NEXT: %[[PAYLOAD:[0-9]+]] = tuple {{.*}}%[[CONVERTED]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[PAYLOAD]]

// INPUT-LABEL: // reverse-mode derivative of branchSubset(_:_:_:_:)
// INPUT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} : $@convention(thin) @Sendable
// INPUT-NEXT: %[[CONVERTED:[0-9]+]] = convert_function %[[ROOT]] to $@callee_guaranteed (Float) -> (Float, Float)
// INPUT-NEXT: // function_ref autodiff subset parameters thunk for pullback
// INPUT-NEXT: %[[THUNK:[0-9]+]] = function_ref
// INPUT-NEXT: %[[ADAPTED:[0-9]+]] = partial_apply {{.*}} %[[THUNK]](%[[CONVERTED]])
// INPUT-NEXT: %[[PAYLOAD:[0-9]+]] = tuple {{.*}}%[[ADAPTED]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[PAYLOAD]]

// INPUT-LABEL: // reverse-mode derivative of branchGeneric<A>(_:_:_:)
// INPUT: // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// INPUT-NEXT: %[[CALLBACK:[0-9]+]] = function_ref
// INPUT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} %[[CALLBACK]]<τ_0_0>({{.*}}) : $@convention(thin) @Sendable
// INPUT-NEXT: %[[CONVERTED:[0-9]+]] = convert_function %[[ROOT]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// INPUT-NEXT: // function_ref thunk for @escaping @callee_guaranteed (@in_guaranteed A) -> (@out A)
// INPUT-NEXT: %[[THUNK:[0-9]+]] = function_ref
// INPUT-NEXT: %[[ADAPTED:[0-9]+]] = partial_apply {{.*}} %[[THUNK]]<τ_0_0>(%[[CONVERTED]])
// INPUT-NEXT: %[[OUTER:[0-9]+]] = convert_function %[[ADAPTED]] to $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// INPUT-NEXT: %[[PAYLOAD:[0-9]+]] = tuple {{.*}}%[[OUTER]]
// INPUT-NEXT: {{.*}} = enum $_AD__{{.*}}, %[[PAYLOAD]]

// Reconstruct the root and its conversion in the specialized pullback. Later
// SILCombine can fold the now-local partial_apply, conversion, and apply.
// DIRECT-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}branchConverted{{.*}}TJp
// DIRECT-NEXT: // specialized pullback of branchConverted(_:_:_:)
// DIRECT: // specialized pullback of branchConverted(_:_:_:)
// DIRECT-NOT: } // end sil function
// DIRECT: // function_ref closure #1 in vjpTagged(_:_:)
// DIRECT-NEXT: %[[CALLBACK:[0-9]+]] = function_ref
// DIRECT-NEXT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} %[[CALLBACK]]({{.*}}) : $@convention(thin) @Sendable
// DIRECT-NEXT: %[[CONVERTED:[0-9]+]] = convert_function %[[ROOT]] to $@callee_guaranteed (Float) -> Float
// DIRECT-NEXT: {{.*}} = apply %[[CONVERTED]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// DIRECT: } // end sil function

// Replay conversions on both sides of the generic reabstraction in the new
// pullback's environment. Generic callback folding is left to later passes.
// GENERIC-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}branchGeneric{{.*}}TJp
// GENERIC-NEXT: // specialized pullback of branchGeneric<A>(_:_:_:)
// GENERIC: // specialized pullback of branchGeneric<A>(_:_:_:)
// GENERIC-NOT: } // end sil function
// GENERIC: // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// GENERIC-NEXT: %[[CALLBACK:[0-9]+]] = function_ref
// GENERIC-NEXT: %[[ROOT:[0-9]+]] = partial_apply {{.*}} %[[CALLBACK]]<τ_0_0>({{.*}}) : $@convention(thin) @Sendable
// GENERIC-NEXT: %[[CONVERTED:[0-9]+]] = convert_function %[[ROOT]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// GENERIC-NEXT: // function_ref thunk for @escaping @callee_guaranteed (@in_guaranteed A) -> (@out A)
// GENERIC-NEXT: %[[THUNK:[0-9]+]] = function_ref
// GENERIC-NEXT: %[[ADAPTED:[0-9]+]] = partial_apply {{.*}} %[[THUNK]]<τ_0_0>(%[[CONVERTED]])
// GENERIC-NEXT: %[[OUTER:[0-9]+]] = convert_function %[[ADAPTED]] to $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// GENERIC-NEXT: {{.*}} = apply %[[OUTER]]({{.*}}) : $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// GENERIC: } // end sil function

// Both final VJPs retain only the top-level pullback partial application.
// The subset variant must also remove the converted inner callback, rather
// than merely specializing the subset thunk around an opaque function.
// FINAL-LABEL: // reverse-mode derivative of branchConverted(_:_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: = convert_function
// FINAL: // function_ref specialized pullback of branchConverted(_:_:_:)
// FINAL-NEXT: %[[FN:[0-9]+]] = function_ref
// FINAL-NEXT: %[[PB:[0-9]+]] = partial_apply {{.*}} %[[FN]]
// FINAL-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PB]])
// FINAL-NEXT: return %[[RESULT]]
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of branchConverted(_:_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function

// FINAL-LABEL: // reverse-mode derivative of branchSubset(_:_:_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: = convert_function
// FINAL: // function_ref specialized pullback of branchSubset(_:_:_:_:)
// FINAL-NEXT: %[[FN:[0-9]+]] = function_ref
// FINAL-NEXT: %[[PB:[0-9]+]] = partial_apply {{.*}} %[[FN]]
// FINAL-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PB]])
// FINAL-NEXT: return %[[RESULT]]
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of branchSubset(_:_:_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function
