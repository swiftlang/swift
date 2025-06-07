struct Foo {
  static let member = Bar() // expected-complete-warning {{static property 'member' is not concurrency-safe because non-'Sendable' type 'Bar' may have shared mutable state; this is an error in the Swift 6 language mode}}
  // expected-complete-note@-1 {{add '@MainActor' to make static property 'member' part of global actor 'MainActor'}}
  // expected-complete-note@-2{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}
}
