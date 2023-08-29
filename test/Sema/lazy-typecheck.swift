// RUN: not %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -experimental-lazy-typecheck

// With lazy typechecking enabled and no compiler outputs specified, no errors
// should be emitted for this semantically invalid code.

var x: DoesNotExist
