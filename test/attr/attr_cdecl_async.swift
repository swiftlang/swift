// RUN: %target-typecheck-verify-swift -enable-objc-interop  -disable-availability-checking

// REQUIRES: concurrency

@_cdecl("async") // expected-error{{@_cdecl global function cannot be asynchronous}}
func asynchronous() async { }

