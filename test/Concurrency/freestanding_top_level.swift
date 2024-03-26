// RUN: %target-swift-frontend -concurrency-model=task-to-thread -typecheck  -verify %s
// RUN: %target-swift-frontend -concurrency-model=task-to-thread -typecheck  -verify -verify-additional-prefix complete- -strict-concurrency=complete %s

// expected-complete-warning@+3 {{var 'global' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in the Swift 6 language mode}}
// expected-complete-note@+2 {{isolate 'global' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
// expected-complete-note@+1 {{var declared here}}
var global = 10

// expected-complete-warning@+1 {{reference to var 'global' is not concurrency-safe because it involves shared mutable state; this is an error in the Swift 6 language mode}}
print(global)
