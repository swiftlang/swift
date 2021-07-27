// RUN: %target-typecheck-verify-swift  -disable-availability-checking

// REQUIRES: concurrency

func f() async -> Int { 0 }

_ = await f() // expected-error{{'async' call in a function that does not support concurrency}}

async let y = await f() // expected-error{{'async let' in a function that does not support concurrency}}
// expected-error@-1{{'async' call in a function that does not support concurrency}}
