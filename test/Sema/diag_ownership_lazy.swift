// RUN: %target-typecheck-verify-swift

class LazyReferenceClass {
  lazy unowned(unsafe) var unsafeSelf = { self }()
  // expected-error@-1 {{lazy properties cannot be 'unowned(unsafe)'}}

  lazy weak var weakValue = LazyReferenceClass()
  // expected-error@-1 {{lazy properties cannot be 'weak'}}

  unowned lazy var unownedValue = LazyReferenceClass()
  // expected-error@-1 {{lazy properties cannot be 'unowned'}}
}
