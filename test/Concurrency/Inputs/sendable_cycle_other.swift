struct Foo {
  static let member = Bar() // expected-complete-warning {{static property 'member' is not concurrency-safe because non-'Sendable' type 'Bar' may have shared mutable state; this is an error in the Swift 6 language mode}}
  // expected-complete-note@-1 {{restrict 'member' to the main actor if it will only be accessed from the main thread}}
  // expected-complete-note@-2{{unsafely mark 'member' as concurrency-safe if all accesses are protected by an external synchronization mechanism}}
}
