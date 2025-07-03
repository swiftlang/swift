// RUN: %target-typecheck-verify-swift -enable-objc-interop \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: concurrency
// REQUIRES: swift_feature_CDecl

@_cdecl("async") // expected-error{{@_cdecl global function cannot be asynchronous}}
func asynchronous() async { }

@cdecl(async2) // expected-error{{@cdecl global function cannot be asynchronous}}
func asynchronous2() async { }

@cdecl(asyncParam)
func asynchronousParam(fn: (String) async -> Int) { }
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{'async' function types cannot be represented in C}}
