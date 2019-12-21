// RUN: %target-typecheck-verify-swift -module-name main -I %S/Inputs -enable-experimental-module-selector

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -module-name main -I %S/Inputs 2>/dev/null

import ModuleSelectorTestingKit

let magnitude: Never = fatalError()

// Test resolution of main:: using `B`

extension main::B: main::Equatable {
  @_implements(main::Equatable, main::==(_:_:)) // expected-error {{name cannot be qualified with module selector here}} {{33-39=}}
  public static func equals(_: main::B, _: main::B) -> main::Bool { main::fatalError() }
  
  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: main::negate())
  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
    // FIXME:
      (main::+) // expected-error {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}} expected-error {{expected expression}} expected-error {{expected expression after operator}}
    let magnitude: main::Int.main::Magnitude = main::magnitude // expected-EVENTUALLY-error {{a type mismatch with 'Never'}} FIXME: expected-error {{variable used within its own initial value}}
    if main::Bool.main::random() {
      main::negate() // expected-EVENTUALLY-error {{something about not finding 'negate' because we didn't look in self}}
    }
    else {
      self = main::B(value: .main::min)
    }
    
    self.main::myNegate()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C: ModuleSelectorTestingKit::Equatable {
  @_implements(ModuleSelectorTestingKit::Equatable, ModuleSelectorTestingKit::==(_:_:)) // expected-error {{name cannot be qualified with module selector here}} {{52-77=}}
  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool { ModuleSelectorTestingKit::fatalError() }
  
  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())
  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
    // FIXME:
      (ModuleSelectorTestingKit::+) // expected-error {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}} expected-error {{expected expression}} expected-error {{expected expression after operator}}
    let magnitude: ModuleSelectorTestingKit::Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude // expected-EVENTUALLY-error {{something about not finding 'magnitude' because we didn't look in self}} FIXME: expected-error {{variable used within its own initial value}}
    if ModuleSelectorTestingKit::Bool.ModuleSelectorTestingKit::random() {
      ModuleSelectorTestingKit::negate() // expected-EVENTUALLY-error {{something about not finding 'negate' because we didn't look in self}}
    }
    else {
      self = ModuleSelectorTestingKit::C(value: .ModuleSelectorTestingKit::min)
    }
    
    self.ModuleSelectorTestingKit::myNegate() // expected-EVENTUALLY-error {{can't find 'myNegate' in ModuleSelectorTestingKit}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`
// FIXME: Many more of these should fail once the feature is actually implemented.

extension Swift::D: Swift::Equatable {
  @_implements(Swift::Equatable, Swift::==(_:_:)) // expected-error {{name cannot be qualified with module selector here}} {{34-41=}}
  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool { Swift::fatalError() }
  
  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: Swift::negate())
  mutating func myNegate() {
    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
    // FIXME:
      (Swift::+) // expected-error {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}} expected-error {{expected expression}} expected-error {{expected expression after operator}}
    let magnitude: Swift::Int.Swift::Magnitude = Swift::magnitude // expected-EVENTUALLY-error {{something about not finding 'magnitude' because we didn't look in self}} FIXME: expected-error {{variable used within its own initial value}}
    if Swift::Bool.Swift::random() {
      Swift::negate() // expected-EVENTUALLY-error {{something about not finding 'negate' because we didn't look in self}}
    }
    else {
      self = Swift::D(value: .ModuleSelectorTestingKit::min)
    }
    
    self.Swift::myNegate() // expected-EVENTUALLY-error {{can't find 'myNegate' in Swift}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

struct AvailableUser {
  @available(macOS 10.15, *) var use1: String { "foo" }

  @main::available() var use2
  @ModuleSelectorTestingKit::available() var use4
  @Swift::available() var use5 // expected-EVENTUALLY-error {{can't find because not in 'Swift'}}
}

func builderUser2(@main::MyBuilder fn: () -> Void) {}
func builderUser3(@ModuleSelectorTestingKit::MyBuilder fn: () -> Void) {}
func builderUser4(@Swift::MyBuilder fn: () -> Void) {} // expected-EVENTUALLY-error {{can't find because not in 'Swift'}}

func whitespace() {
  Swift::print // expected-error{{expression resolves to an unused function}}

  Swift :: print // expected-error{{expression resolves to an unused function}}

  Swift::
  print // expected-error{{expression resolves to an unused function}}

  Swift
  ::print // expected-error{{expression resolves to an unused function}}

  Swift ::
  print // expected-error{{expression resolves to an unused function}}

  Swift
  :: print // expected-error{{expression resolves to an unused function}}

  Swift
  ::
  print // expected-error{{expression resolves to an unused function}}
}
