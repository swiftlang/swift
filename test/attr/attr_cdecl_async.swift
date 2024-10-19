// RUN: %target-typecheck-verify-swift -enable-objc-interop -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

@_cdecl("async") // expected-error{{@_cdecl global function cannot be asynchronous}}
func asynchronous() async { }

