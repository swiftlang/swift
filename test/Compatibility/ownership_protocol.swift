// RUN: %target-typecheck-verify-swift -swift-version 4
// Generate a swift 4 compatible warning if ownership is specified in a protocol

class SomeClass {}

protocol P {
  // expected-warning@+1 {{'weak' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  weak var foo: SomeClass? { get set }
  // expected-warning@+1 {{'unowned' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  unowned var foo2: SomeClass { get set }
  // expected-warning@+2 {{'weak' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  // expected-error@+1 {{'weak' may only be applied to class and class-bound protocol types, not 'Int'}}
  weak var foo3: Int? { get set }
  // expected-warning@+2 {{'unowned' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  // expected-error@+1 {{'unowned' may only be applied to class and class-bound protocol types, not 'Int'}}
  unowned var foo4: Int { get set }
}

