// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/main.swift %t/foo.swift -swift-version 6 -verify-additional-prefix swift6-
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift %t/foo.swift -swift-version 5 -verify-additional-prefix swift5-

//--- foo.swift
func foo() -> Int {
  // expected-swift6-note@-1 {{add '@MainActor' to make global function 'foo()' part of global actor 'MainActor'}}
  a + 10
  // expected-swift6-error@-1 {{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
}

//--- main.swift
var a = 10 // expected-swift6-note 2{{var declared here}}

@MainActor
var b = 14 // expected-error {{top-level code variables cannot have a global actor}}

func nonIsolatedSynchronous() {
  // expected-swift6-note@-1 {{add '@MainActor' to make global function 'nonIsolatedSynchronous()' part of global actor 'MainActor'}}
    print(a)
// expected-swift6-error@-1 {{main actor-isolated var 'a' can not be referenced from a nonisolated context}}
}

func nonIsolatedAsync() async {
  print(a)
  // expected-swift6-error@-1 {{main actor-isolated var 'a' cannot be accessed from outside of the actor}}
  // expected-swift5-warning@-2 {{main actor-isolated var 'a' cannot be accessed from outside of the actor}}
}

await nonIsolatedAsync()

@MainActor
func isolated() {
  print(a)
}

func asyncFun() async {
  await print(a)
}

await asyncFun()
