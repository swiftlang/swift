struct Foo {
  static let member = Bar() // expected-complete-warning {{static property 'member' is not concurrency-safe because non-'Sendable' type 'Bar' may have shared mutable state; this is an error in the Swift 6 language mode}}
  // expected-complete-note@-1 {{isolate 'member' to a global actor, or conform 'Bar' to 'Sendable'}}
}
