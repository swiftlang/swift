// Verifies that all of the generic signatures in the standard library are
// minimal and canonical.

// RUN: %target-typecheck-verify-swift -typecheck %s -verify-generic-signatures Swift

// expected-no-diagnostics
