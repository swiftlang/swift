// RUN: %target-swift-frontend -typecheck -disable-availability-checking -enable-experimental-async-top-level -swift-version 6 %s -verify
// RUN: %target-swift-frontend -typecheck -disable-availability-checking -swift-version 6 %s -verify

// REQUIRES: asserts

// Even though enable-experimental-async-top-level is enabled, there are no
// 'await's made from the top-level, thus the top-level is not an asynchronous
// context. `a` is just a normal top-level global variable with no actor
// isolation.

var a = 10 // expected-note 15 {{var declared here}}

func nonIsolatedSync() {
    print(a) // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
    a = a + 10 // expected-warning 2 {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
}

@MainActor
func isolatedSync() { // expected-note 2 {{calls to global function 'isolatedSync()' from outside of its actor context are implicitly asynchronous}}
    print(a) // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
    a = a + 10 // expected-warning 2 {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
}

func nonIsolatedAsync() async {
    await print(a) // expected-warning {{no 'async' operations occur within 'await' expression}}
    // expected-warning@-1 {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
    a = a + 10 // expected-warning 2 {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
}

@MainActor
func isolatedAsync() async { // expected-note 2 {{calls to global function 'isolatedAsync()' from outside of its actor context are implicitly asynchronous}}
    print(a) // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
    a = a + 10 // expected-warning 2 {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
}

nonIsolatedSync()
isolatedSync() // expected-error {{call to main actor-isolated global function 'isolatedSync()' in a synchronous nonisolated context}}
nonIsolatedAsync() // expected-error {{'async' call in a function that does not support concurrency}}
isolatedAsync() // expected-error {{call to main actor-isolated global function 'isolatedAsync()' in a synchronous nonisolated context}}
// expected-error@-1 {{'async' call in a function that does not support concurrency}}

print(a) // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}

if a > 10 { // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
    nonIsolatedSync()
    isolatedSync() // expected-error {{call to main actor-isolated global function 'isolatedSync()' in a synchronous nonisolated context}}
    nonIsolatedAsync() // expected-error {{'async' call in a function that does not support concurrency}}
    isolatedAsync() // expected-error {{call to main actor-isolated global function 'isolatedAsync()' in a synchronous nonisolated context}}
    // expected-error@-1 {{'async' call in a function that does not support concurrency}}

    print(a) // expected-warning {{reference to var 'a' is not concurrency-safe because it involves shared mutable state}}
}
