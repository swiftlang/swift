// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// -verify-ignore-unknown is for:
// <unknown>:0: error: unexpected note produced: 'self' declared here

class LazyReferenceClass {
  // expected-note@-1 {{type declared here}}
  // expected-note@-2 {{type declared here}}
  lazy unowned(unsafe) var unsafeSelf = { self }()
  // expected-error@-1 {{lazy properties cannot be 'unowned(unsafe)'}}

  lazy weak var weakSelf = self
  // expected-error@-1 {{lazy properties cannot be 'weak'}}
  // expected-error@-2 {{class declaration cannot close over value 'self' defined in outer scope}}

  unowned lazy var unownedSelf = self
  // expected-error@-1 {{lazy properties cannot be 'unowned'}}
  // expected-error@-2 {{class declaration cannot close over value 'self' defined in outer scope}}
}
