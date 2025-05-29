// RUN: %target-swift-frontend -typecheck -target %target-swift-5.1-abi-triple -enable-experimental-async-top-level -swift-version 6 %s -verify

var a = 10
// expected-note@-1 2 {{var declared here}}
// expected-note@-2 2 {{mutation of this var is only permitted within the actor}}

func nonIsolatedSync() { //expected-note 3 {{add '@MainActor' to make global function 'nonIsolatedSync()' part of global actor 'MainActor'}}
    print(a)    // expected-error {{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
    a = a + 10
    // expected-error@-1:9 {{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
    // expected-error@-2:5 {{main actor-isolated var 'a' can not be mutated from a nonisolated context}}

}

@MainActor
func isolatedSync() {
    print(a)
    a = a + 10
}

func nonIsolatedAsync() async {
    await print(a)
    a = a + 10
    // expected-error@-1:5 {{main actor-isolated var 'a' can not be mutated from a nonisolated context}}
    // expected-error@-2:9 {{main actor-isolated var 'a' cannot be accessed from outside of the actor}} {{9-9=await }}
    // expected-note@-3 {{consider declaring an isolated method on 'MainActor' to perform the mutation}}
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
