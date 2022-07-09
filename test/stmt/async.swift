// RUN: %target-typecheck-verify-swift  -disable-availability-checking

// REQUIRES: concurrency

@_silgen_name("omnomInt")
func omnom(_ x: Int)

func f() async -> Int { 0 }

func syncContext() {        // expected-note 3 {{add 'async' to function 'syncContext()' to make it asynchronous}}
    _ = await f()           // expected-error{{'async' call in a function that does not support concurrency}}
    async let y = await f() // expected-error{{'async let' in a function that does not support concurrency}}
    await omnom(y)          // expected-error{{'async let' in a function that does not support concurrency}}
}
