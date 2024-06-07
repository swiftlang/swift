// RUN: %target-typecheck-verify-swift -swift-version 4
// Generate a swift 4 compatible warning if ownership is specified in a protocol

class SomeClass {}

protocol P {
  // expected-warning@+1 {{'weak' cannot be applied to a property declaration in a protocol; this is an error in the Swift 5 language mode}}
  weak var foo: SomeClass? { get set }
  // expected-warning@+1 {{'unowned' cannot be applied to a property declaration in a protocol; this is an error in the Swift 5 language mode}}
  unowned var foo2: SomeClass { get set }
  // expected-warning@+2 {{'weak' cannot be applied to a property declaration in a protocol; this is an error in the Swift 5 language mode}}
  // expected-error@+1 {{'weak' may only be applied to class and class-bound protocol types, not 'Int'}}
  weak var foo3: Int? { get set }
  // expected-warning@+2 {{'unowned' cannot be applied to a property declaration in a protocol; this is an error in the Swift 5 language mode}}
  // expected-error@+1 {{'unowned' may only be applied to class and class-bound protocol types, not 'Int'}}
  unowned var foo4: Int { get set }
}

