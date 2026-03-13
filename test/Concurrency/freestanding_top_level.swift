// RUN: %target-swift-frontend -concurrency-model=task-to-thread -typecheck  -verify %s
// RUN: %target-swift-frontend -concurrency-model=task-to-thread -typecheck  -verify -verify-additional-prefix complete- -strict-concurrency=complete %s

// expected-complete-warning@+4 {{var 'global' is not concurrency-safe because it is nonisolated global shared mutable state; this is an error in the Swift 6 language mode}}
// expected-complete-note@+3 {{add '@MainActor' to make var 'global' part of global actor 'MainActor'}}{{1-1=@MainActor }}
// expected-complete-note@+2 {{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}{{1-1=nonisolated(unsafe) }}
// expected-complete-note@+1 {{convert 'global' to a 'let' constant to make 'Sendable' shared state immutable}}{{1-4=let}}
var global = 10

// No warning because we're in the same module.
print(global)
