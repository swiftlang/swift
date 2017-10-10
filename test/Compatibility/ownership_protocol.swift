// RUN: %target-typecheck-verify-swift -swift-version 4
// Generate a swift 4 compatible warning if ownership is specified in a protocol

class SomeClass {}

protocol P {
  weak var foo: SomeClass? { get set } // expected-warning {{'weak' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  unowned var foo2: SomeClass { get set } // expected-warning {{'unowned' should not be applied to a property declaration in a protocol and will be disallowed in future versions}}
  weak var foo3: Int? { get set } // expected-error {{'weak' may only be applied to class and class-bound protocol types, not 'Int'}}
  unowned var foo4: Int { get set } // expected-error {{'unowned' may only be applied to class and class-bound protocol types, not 'Int'}}
}

