// RUN: %target-typecheck-verify-swift -enable-objc-interop -disable-availability-checking -enable-experimental-feature CDecl

// REQUIRES: concurrency
// REQUIRES: swift_feature_CDecl

@_cdecl("async") // expected-error{{@_cdecl global function cannot be asynchronous}}
func asynchronous() async { }

@cdecl("async2") // expected-error{{@cdecl global function cannot be asynchronous}}
func asynchronous2() async { }

