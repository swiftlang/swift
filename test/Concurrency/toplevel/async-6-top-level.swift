// RUN: %target-swift-frontend -typecheck -disable-availability-checking -enable-experimental-async-top-level -swift-version 6 %s -verify

// REQUIRES: asserts

var a = 10
// expected-note@-1 2 {{var declared here}}
// expected-note@-2 2 {{mutation of this var is only permitted within the actor}}

func nonIsolatedSync() { //expected-note 3 {{add '@MainActor' to make global function 'nonIsolatedSync()' part of global actor 'MainActor'}}
    print(a)    // expected-error {{var 'a' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
    a = a + 10
    // expected-error@-1:5 {{var 'a' isolated to global actor 'MainActor' can not be mutated from this context}}
    // expected-error@-2:9 {{var 'a' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}

}

@MainActor
func isolatedSync() {
    print(a)
    a = a + 10
}

func nonIsolatedAsync() async {
    await print(a)
    a = a + 10
    // expected-error@-1:5 {{var 'a' isolated to global actor 'MainActor' can not be mutated from this context}}
    // expected-error@-2:9 {{expression is 'async' but is not marked with 'await'}}
    // expected-note@-3:9 {{property access is 'async'}}
}

@MainActor
func isolatedAsync() async {
    print(a)
    a = a + 10
}

nonIsolatedSync()
isolatedSync()
await nonIsolatedAsync()
await isolatedAsync()

print(a)

if a > 10 {
    nonIsolatedSync()
    isolatedSync()
    await nonIsolatedAsync()
    await isolatedAsync()

    print(a)
}
