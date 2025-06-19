// RUN: %target-swift-frontend -typecheck -target %target-swift-5.1-abi-triple -swift-version 6 %s -verify

// Even though enable-experimental-async-top-level is enabled, there are no
// 'await's made from the top-level, thus the top-level is not an asynchronous
// context. `a` is just a normal top-level global variable with no actor
// isolation.

var a = 10 // expected-note 2 {{var declared here}}
// expected-note@-1 2{{mutation of this var is only permitted within the actor}}

// expected-note@+1 3{{add '@MainActor' to make global function 'nonIsolatedSync()' part of global actor 'MainActor'}}
func nonIsolatedSync() {
    print(a) // expected-error {{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
    a = a + 10 // expected-error{{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
  // expected-error@-1{{main actor-isolated var 'a' can not be mutated from a nonisolated context}}
}

@MainActor
func isolatedSync() {
    print(a)
    a = a + 10
}

func nonIsolatedAsync() async {
  await print(a)
  a = a + 10 // expected-error{{main actor-isolated var 'a' can not be mutated from a nonisolated context}}
  // expected-error@-1{{main actor-isolated var 'a' cannot be accessed from outside of the actor}} {{7-7=await }}
  // expected-note@-2{{consider declaring an isolated method on 'MainActor' to perform the mutation}}
}

@MainActor
func isolatedAsync() async {
    print(a)
    a = a + 10
}

nonIsolatedSync()
isolatedSync()
nonIsolatedAsync() // expected-error {{'async' call in a function that does not support concurrency}}
isolatedAsync()
// expected-error@-1 {{'async' call in a function that does not support concurrency}}

print(a)

if a > 10 {
    nonIsolatedSync()
    isolatedSync()
    nonIsolatedAsync() // expected-error {{'async' call in a function that does not support concurrency}}
    isolatedAsync()
    // expected-error@-1 {{'async' call in a function that does not support concurrency}}

    print(a)
}
