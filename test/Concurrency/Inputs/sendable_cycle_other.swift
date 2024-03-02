struct Foo {
  static let member = Bar() // expected-complete-warning {{static property 'member' is not concurrency-safe because it is not either conforming to 'Sendable' or isolated to a global actor; this is an error in the Swift 6 language mode}}
}
