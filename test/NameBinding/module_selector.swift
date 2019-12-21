// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-module-selector

// FIXME: This only works with ASTScopes, and we might not care about that by the time this feature is ready.
// RUN-DISABLED: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-module-selector -disable-astscope-lookup

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs 2>/dev/null

import ModuleSelectorTestingKit

import ctypes::bits
import struct ModuleSelectorTestingKit::A

let magnitude: Never = fatalError()

// Test resolution of main:: using `B`

extension main::B {}
// FIXME improve: expected-error@-1 {{use of undeclared type 'main::B'}}

extension B: main::Equatable {
  @_implements(main::Equatable, main::==(_:_:))
  // expected-error@-1 {{name cannot be qualified with module selector here}} {{33-39=}}
  public static func equals(_: main::B, _: main::B) -> main::Bool { main::fatalError() }
  
  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: main::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'main::negate()' could not be found}}
  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
    // FIXME:
      (main::+)
      // expected-error@-1 {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}}
      // expected-error@-2 {{expected expression}}
      // expected-error@-3 {{expected expression after operator}}

    let magnitude: Int.main::Magnitude = main::magnitude
    // expected-EVENTUALLY-error@-1 {{can't find 'Int'}}
    // FIXME improve: expected-error@-2 {{type alias 'Magnitude' is not a member type of 'Int'}}
    // FIXME: expected-error@-3 {{variable used within its own initial value}}
    if main::Bool.main::random() {
      main::negate()
      // FIXME improve: expected-error@-1 {{use of unresolved identifier 'main::negate'}}
    }
    else {
      self = main::B(value: .main::min)
    }
    
    self.main::myNegate()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension C {}

extension ModuleSelectorTestingKit::C: ModuleSelectorTestingKit::Equatable {
// FIXME improve: expected-error@-1 {{use of undeclared type 'ModuleSelectorTestingKit::Equatable'}}
  @_implements(ModuleSelectorTestingKit::Equatable, ModuleSelectorTestingKit::==(_:_:))
  // FIXME improve: expected-error@-1 {{use of undeclared type 'ModuleSelectorTestingKit::Equatable'}}
  // expected-error@-2 {{name cannot be qualified with module selector here}} {{52-77=}}
  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool { ModuleSelectorTestingKit::fatalError() }
  // FIXME improve: expected-error@-1 {{use of undeclared type 'ModuleSelectorTestingKit::Bool'}}

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())
  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
    // FIXME improve: expected-error@-1 3{{use of undeclared type 'ModuleSelectorTestingKit::Int'}}
    // FIXME:
      (ModuleSelectorTestingKit::+)
      // expected-error@-1 {{expected expression}}
      // expected-error@-2 {{expected expression after operator}}
    let magnitude: Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude
    // expected-EVENTUALLY-error@-1 {{something about not finding 'magnitude' because we didn't look in self}}
    // FIXME improve: expected-error@-2 {{type alias 'Magnitude' is not a member type of 'Int'}}
    // FIXME: expected-error@-3 {{variable used within its own initial value}}
    if ModuleSelectorTestingKit::Bool.ModuleSelectorTestingKit::random() {
    // FIXME improve: expected-error@-1 {{use of unresolved identifier 'ModuleSelectorTestingKit::Bool'}}
      ModuleSelectorTestingKit::negate()
      // FIXME improve, suggest adding 'self.': expected-error@-1 {{use of unresolved identifier 'ModuleSelectorTestingKit::negate'}}
    }
    else {
      self = ModuleSelectorTestingKit::C(value: .ModuleSelectorTestingKit::min)
    }
    
    self.ModuleSelectorTestingKit::myNegate()
    // expected-EVENTUALLY-error@-2 {{can't find 'myNegate' in ModuleSelectorTestingKit}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`
// FIXME: Many more of these should fail once the feature is actually implemented.

extension Swift::D {}
// FIXME improve: expected-error@-1 {{use of undeclared type 'Swift::D'}}

extension D: Swift::Equatable {
  @_implements(Swift::Equatable, Swift::==(_:_:))
  // expected-error@-1 {{name cannot be qualified with module selector here}} {{34-41=}}
  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool { Swift::fatalError() }
  
  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)
  
  @_dynamicReplacement(for: Swift::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'Swift::negate()' could not be found}}
  mutating func myNegate() {
    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
    // FIXME:
      (Swift::+)
      // expected-error@-1 {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}}
      // expected-error@-2 {{expected expression}}
      // expected-error@-3 {{expected expression after operator}}
    let magnitude: Int.Swift::Magnitude = Swift::magnitude
    // expected-EVENTUALLY-error@-1 {{something about not finding 'magnitude' because we didn't look in self}}
    // FIXME: expected-error@-2 {{variable used within its own initial value}}
    if Swift::Bool.Swift::random() {
      Swift::negate()
      // FIXME improve: expected-error@-1 {{use of unresolved identifier 'Swift::negate'}}
    }
    else {
      self = Swift::D(value: .ModuleSelectorTestingKit::min)
    }
    
    self.Swift::myNegate()
    // expected-EVENTUALLY-error@-1 {{can't find 'myNegate' in Swift}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

struct AvailableUser {
  @available(macOS 10.15, *) var use1: String { "foo" }

  @main::available() var use2
  // FIXME improve: expected-error@-1 {{unknown attribute 'available'}}
  // FIXME suppress: expected-error@-2 {{type annotation missing in pattern}}

  @ModuleSelectorTestingKit::available() var use4
  // no-error

  @Swift::available() var use5
  // FIXME improve: expected-error@-1 {{unknown attribute 'available'}}
  // FIXME suppress: expected-error@-2 {{type annotation missing in pattern}}
}

func builderUser2(@main::MyBuilder fn: () -> Void) {}
// FIXME improve: expected-error@-1 {{unknown attribute 'MyBuilder'}}

func builderUser3(@ModuleSelectorTestingKit::MyBuilder fn: () -> Void) {}
// no-error

func builderUser4(@Swift::MyBuilder fn: () -> Void) {}
// FIXME improve: expected-error@-1 {{unknown attribute 'MyBuilder'}}

func whitespace() {
  Swift::print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift:: print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift ::print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift :: print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift::
  print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift
  ::print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift ::
  print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift
  :: print
  // expected-error@-1 {{expression resolves to an unused function}}

  Swift
  ::
  print
  // expected-error@-1 {{expression resolves to an unused function}}
}

// Error cases

func main::decl1(
  // expected-error@-1 {{name of function declaration cannot be qualified with module selector}}
  main::p1: main::A,
  // expected-error@-1 {{argument label cannot be qualified with module selector}}
  // FIXME access path: expected-error@-2 {{use of undeclared type 'main::A'}}
  main::label p2: main::A,
  // expected-error@-1 {{argument label cannot be qualified with module selector}}
  // FIXME access path: expected-error@-2 {{use of undeclared type 'main::A'}}
  label main::p3: main::A
  // expected-error@-1 {{name of parameter declaration cannot be qualified with module selector}}
  // FIXME access path: expected-error@-2 {{use of undeclared type 'main::A'}}
) {
  let main::decl1a = "a"
  // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}

  var main::decl1b = "b"
  // expected-error@-1 {{name of variable declaration cannot be qualified with module selector}}

  let (main::decl1c, main::decl1d) = ("c", "d")
  // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
  // expected-error@-2 {{name of constant declaration cannot be qualified with module selector}}

  if let (main::decl1e, main::decl1f) = Optional(("e", "f")) {}
  // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
  // expected-error@-2 {{name of constant declaration cannot be qualified with module selector}}

  guard let (main::decl1g, main::decl1h) = Optional(("g", "h")) else { return }
  // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
  // expected-error@-2 {{name of constant declaration cannot be qualified with module selector}}

  // From uses in the switch statements below:
  // expected-note@-5 3{{did you mean 'decl1g'?}}

  switch Optional(main::decl1g) {
  // expected-error@-1 {{use of unresolved identifier 'main::decl1g'}}
  case Optional.some(let main::decl1i):
    // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  // expected-error@-1 {{use of unresolved identifier 'main::decl1g'}}
  case let Optional.some(main::decl1j):
    // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
 // expected-error@-1 {{use of unresolved identifier 'main::decl1g'}}
 case let main::decl1k?:
    // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}
    break
  case .none:
    break
  }

  for main::decl1l in "lll" {}
  // expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}

  "lll".forEach { [main::magnitude]
    // expected-error@-1 {{name of captured variable declaration cannot be qualified with module selector}}
    main::elem in print(elem)
    // expected-error@-1 {{name of parameter declaration cannot be qualified with module selector}}
  }

  "lll".forEach { (main::elem) in print(elem) }
  // expected-error@-1 {{name of parameter declaration cannot be qualified with module selector}}

  "lll".forEach { (main::elem) -> Void in print(elem) }
  // expected-error@-1 {{name of parameter declaration cannot be qualified with module selector}}

  "lll".forEach { (main::elem: Character) -> Void in print(elem) }
  // expected-error@-1 {{name of parameter declaration cannot be qualified with module selector}}
}
enum main::decl2 {
  // expected-error@-1 {{name of enum declaration cannot be qualified with module selector}}

  case main::decl2a
  // expected-error@-1 {{name of enum 'case' declaration cannot be qualified with module selector}}
}

struct main::decl3 {}
// expected-error@-1 {{name of struct declaration cannot be qualified with module selector}}

class main::decl4<main::T> {}
// expected-error@-1 {{name of class declaration cannot be qualified with module selector}}
// expected-error@-2 {{name of generic parameter declaration cannot be qualified with module selector}}

typealias main::decl5 = main::Bool
// expected-error@-1 {{name of typealias declaration cannot be qualified with module selector}}
// FIXME improve: expected-error@-2 {{use of undeclared type 'main::Bool'}}

protocol main::decl6 {
  // expected-error@-1 {{name of protocol declaration cannot be qualified with module selector}}

  associatedtype main::decl6a
  // expected-error@-1 {{name of associatedtype declaration cannot be qualified with module selector}}
}

let main::decl7 = 7
// expected-error@-1 {{name of constant declaration cannot be qualified with module selector}}

var main::decl8 = 8 {
// expected-error@-1 {{name of variable declaration cannot be qualified with module selector}}

  willSet(main::newValue) {}
  // expected-error@-1 {{name of accessor parameter declaration cannot be qualified with module selector}}

  didSet(main::oldValue) {}
  // expected-error@-1 {{name of accessor parameter declaration cannot be qualified with module selector}}
}

struct Parent {
  func main::decl1() {}
  // expected-error@-1 {{name of function declaration cannot be qualified with module selector}}

  enum main::decl2 {
  // expected-error@-1 {{name of enum declaration cannot be qualified with module selector}}

    case main::decl2a
    // expected-error@-1 {{name of enum 'case' declaration cannot be qualified with module selector}}
  }

  struct main::decl3 {}
  // expected-error@-1 {{name of struct declaration cannot be qualified with module selector}}

  class main::decl4 {}
  // expected-error@-1 {{name of class declaration cannot be qualified with module selector}}

  typealias main::decl5 = main::Bool
  // expected-error@-1 {{name of typealias declaration cannot be qualified with module selector}}
  // FIXME improve: expected-error@-2 {{use of undeclared type 'main::Bool'}}
}

@_swift_native_objc_runtime_base(main::BaseClass)
// expected-error@-1 {{Objective-C class name in @_swift_native_objc_runtime_base}}
class C1 {}

infix operator <<<<< : Swift::AdditionPrecedence
// expected-error@-1 {{precedence group specifier cannot be qualified with module selector}}

precedencegroup main::PG1 {
// expected-error@-1 {{name of precedence group declaration cannot be qualified with module selector}}

  higherThan: Swift::AdditionPrecedence
  // expected-error@-1 {{precedence group specifier cannot be qualified with module selector}}
}
