// RUN: %target-typecheck-verify-swift

class LazyReferenceClass {
  lazy unowned(unsafe) var unsafeSelf = { self }()
  // expected-error@-1 {{lazy properties cannot be 'unowned(unsafe)'}}

  lazy weak var weakValue = LazyReferenceClass()
  // expected-error@-1 {{lazy properties cannot be 'weak'}}
  // expected-warning@-2 {{instance will be immediately deallocated because property 'weakValue' is 'weak'}}
  // expected-note@-3 {{'weakValue' declared here}}
  // expected-note@-4 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned lazy var unownedValue = LazyReferenceClass()
  // expected-error@-1 {{lazy properties cannot be 'unowned'}}
  // expected-warning@-2 {{instance will be immediately deallocated because property 'unownedValue' is 'unowned'}}
  // expected-note@-3 {{'unownedValue' declared here}}
  // expected-note@-4 {{a strong reference is required to prevent the instance from being deallocated}}
}
