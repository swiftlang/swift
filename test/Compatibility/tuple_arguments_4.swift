// RUN: %target-typecheck-verify-swift -swift-version 4

// https://bugs.swift.org/browse/SR-6837

// FIXME: Can't overlaod local functions so these must be top-level
func takePairOverload(_ pair: (Int, Int?)) {}
func takePairOverload(_: () -> ()) {}

do {
  func takeFn(fn: (_ i: Int, _ j: Int?) -> ()) {}
  func takePair(_ pair: (Int, Int?)) {}
  takeFn(fn: takePair) // Allow for -swift-version 4, but not later
  takeFn(fn: takePairOverload) // Allow for -swift-version 4, but not later

  takeFn(fn: { (pair: (Int, Int?)) in } ) // Disallow for -swift-version 4 and later
  // expected-error@-1 {{contextual closure type '(Int, Int?) -> ()' expects 2 arguments, but 1 was used in closure body}}
  takeFn { (pair: (Int, Int?)) in } // Disallow for -swift-version 4 and later
  // expected-error@-1 {{contextual closure type '(Int, Int?) -> ()' expects 2 arguments, but 1 was used in closure body}}
}

// https://bugs.swift.org/browse/SR-6796
do {
  func f(a: (() -> Void)? = nil) {}
  func log<T>() -> ((T) -> Void)? { return nil }

  f(a: log() as ((()) -> Void)?) // Allow ((()) -> Void)? to be passed in place of (() -> Void)? for -swift-version 4 but not later.

  func logNoOptional<T>() -> (T) -> Void { }
  f(a: logNoOptional() as ((()) -> Void)) // Also allow the optional-injected form.

  func g() {}
  g(()) // expected-error {{argument passed to call that takes no arguments}}

  func h(_: ()) {} // expected-note {{'h' declared here}}
  h() // expected-error {{missing argument for parameter #1 in call}}
}
