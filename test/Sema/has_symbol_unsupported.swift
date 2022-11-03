// RUN: %target-typecheck-verify-swift
// UNSUPPORTED: VENDOR=apple

// Verify that #_hasSymbol is rejected on non-Darwin platforms

func foo() {}

if #_hasSymbol(foo) { } // expected-error {{'#_hasSymbol' is unsupported on target}}
