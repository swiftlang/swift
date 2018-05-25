// RUN: %target-typecheck-verify-swift -swift-version 5

class SomeClass {}

protocol P {
  // expected-error@+1 {{'weak' cannot be applied to a property declaration in a protocol}}
  weak var foo: SomeClass? { get set }
  // expected-error@+1 {{'unowned' cannot be applied to a property declaration in a protocol}}
  unowned var foo2: SomeClass { get set }
  // expected-error@+2 {{'weak' cannot be applied to a property declaration in a protocol}}
  // expected-error@+1 {{'weak' may only be applied to class and class-bound protocol types, not 'Int'}}
  weak var foo3: Int? { get set }
  // expected-error@+2 {{'unowned' cannot be applied to a property declaration in a protocol}}
  // expected-error@+1 {{'unowned' may only be applied to class and class-bound protocol types, not 'Int'}}
  unowned var foo4: Int { get set }
  var foo9: SomeClass? { get }
}

extension P {
  weak var foo5: SomeClass? // expected-error {{extensions must not contain stored properties}}
  unowned var foo6: SomeClass // expected-error {{extensions must not contain stored properties}}
  
  weak var foo7: SomeClass? { // Okay
    return SomeClass()
  }

  unowned var foo8: SomeClass { // Okay
    return SomeClass()
  }

  weak var foo9: SomeClass? { // Okay
    return nil
  }
}

