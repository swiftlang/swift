// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

func f() async -> Int { 0 }

_ = await f() // expected-error{{'async' in a function that does not support concurrency}}

async let y = await f() // expected-error{{'async let' in a function that does not support concurrency}}
// expected-error@-1{{'async' in a function that does not support concurrency}}
