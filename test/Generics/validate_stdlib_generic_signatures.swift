// Verifies that all of the generic signatures in the standard library are
// minimal and canonical.

// RUN: not %target-typecheck-verify-swift -verify-generic-signatures Swift 2>&1 | %FileCheck %s

// CHECK-NOT: error:
// CHECK-DAG: error: unexpected error produced: generic requirement 'τ_0_0.Index : Strideable' is redundant in <τ_0_0 where τ_0_0 : RandomAccessCollection, τ_0_0.Index : Strideable, τ_0_0.Indices == Range<τ_0_0.Index>, τ_0_0.Index.Stride == Int>
// CHECK-DAG: error: diagnostic produced elsewhere: generic requirement 'τ_0_0.Index : Strideable' is redundant in <τ_0_0 where τ_0_0 : RandomAccessCollection, τ_0_0.Index : Strideable, τ_0_0.Indices == Range<τ_0_0.Index>, τ_0_0.Index.Stride == Int>
// CHECK-NOT: error:
