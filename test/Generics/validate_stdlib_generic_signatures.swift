// Verifies that all of the generic signatures in the standard library are
// minimal and canonical.

// RUN: not %target-typecheck-verify-swift -verify-generic-signatures Swift 2> %t.log

// RUN: grep -c "error:" %t.log | count 1
// RUN: %FileCheck %s < %t.log

// CHECK: error: unexpected error produced: generic requirement 'τ_0_0.Index : Strideable' is redundant in <τ_0_0 where τ_0_0 : RandomAccessCollection, τ_0_0.Index : Strideable, τ_0_0.Indices == Range<τ_0_0.Index>, τ_0_0.Index.Stride == Int>

