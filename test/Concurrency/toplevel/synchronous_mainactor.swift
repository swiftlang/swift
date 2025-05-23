// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-experimental-async-top-level -strict-concurrency=complete -typecheck -verify %s

var a = 10 // expected-note{{var declared here}}

@MainActor
var b = 15 // expected-error{{top-level code variables cannot have a global actor}}

func unsafeAccess() { // expected-note{{add '@MainActor' to make global function 'unsafeAccess()' part of global actor 'MainActor'}}
    print(a) // expected-error@:11{{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
}

func unsafeAsyncAccess() async {
    print(a) // expected-error@:11{{main actor-isolated var 'a' cannot be accessed from outside of the actor}}{{5-5=await }}
}

@MainActor
func safeAccess() {
    print(a)
}

@MainActor
func safeSyncAccess() async {
    print(a)
}

func safeAsyncAccess() async {
    await print(a)
}

print(a)
