// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-feature ModuleSelector

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs 2>/dev/null

// REQUIRES: swift_feature_ModuleSelector

// FIXME: This test doesn't really cover:
//
// * Whether X::foo finds foos in X's re-exports
// * Whether we handle access paths correctly
// * Interaction with ClangImporter
// * Cross-import overlays
// * Key paths
// * Key path dynamic member lookup
// * Custom type attributes (and coverage of type attrs generally is sparse)
// * Macros
//
// It also might not cover all combinations of name lookup paths and inputs.

import ModuleSelectorTestingKit

import ctypes::bits   // FIXME: ban using :: with submodules?
import struct ModuleSelectorTestingKit::A

let magnitude: Never = fatalError()

// Test correct code using `A`

extension ModuleSelectorTestingKit::A {}

extension A: @retroactive Swift::Equatable {
  @_implements(Swift::Equatable, ==(_:_:))
  public static func equals(_: ModuleSelectorTestingKit::A, _: ModuleSelectorTestingKit::A) -> Swift::Bool {
    Swift::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())
  mutating func myNegate() {
    let fn: (Swift::Int, Swift::Int) -> Swift::Int = (Swift::+)

    let magnitude: Int.Swift::Magnitude = main::magnitude
    // expected-error@-1 {{cannot convert value of type 'Never' to specified type 'Int.Magnitude' (aka 'UInt')}}

    _ = (fn, magnitude)

    if Swift::Bool.Swift::random() {
      self.ModuleSelectorTestingKit::negate()
    }
    else {
      self = ModuleSelectorTestingKit::A(value: .Swift::min)
      self = A.ModuleSelectorTestingKit::init(value: .min)
    }

    self.main::myNegate()

    Swift::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of main:: using `B`

extension main::B {}
// FIXME improve: expected-error@-1 {{cannot find type 'main::B' in scope}}

extension B: @retroactive main::Equatable {
  // FIXME improve: expected-error@-1 {{cannot find type 'main::Equatable' in scope}}

  @_implements(main::Equatable, main::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{33-39=}}
  // FIXME improve: expected-error@-2 {{cannot find type 'main::Equatable' in scope}}

  public static func equals(_: main::B, _: main::B) -> main::Bool {
  // FIXME improve: expected-error@-1 {{cannot find type 'main::B' in scope}}
  // FIXME improve: expected-error@-2 {{cannot find type 'main::B' in scope}}
  // FIXME improve: expected-error@-3 {{cannot find type 'main::Bool' in scope}}
    main::fatalError() // no-error -- not typechecking function bodies
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'main::negate()' could not be found}}

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
    // FIXME improve: expected-error@-1 3{{cannot find type 'main::Int' in scope}}
      (main::+)
      // FIXME: should fail????

    let magnitude: Int.main::Magnitude = main::magnitude
    // FIXME improve: expected-error@-1 {{'main::Magnitude' is not a member type of struct 'Swift.Int'}}

    _ = (fn, magnitude)

    if main::Bool.main::random() {
      // FIXME improve: expected-error@-1 {{cannot find 'main::Bool' in scope}}

      main::negate()
      // FIXME improve: expected-error@-1 {{cannot find 'main::negate' in scope}}
    }
    else {
      self = main::B(value: .main::min)
      // FIXME improve: expected-error@-1 {{cannot find 'main::B' in scope}}
      // expected-error@-2 {{cannot infer contextual base in reference to member 'main::min'}}

      self = B.main::init(value: .min)
      // FIXME improve: expected-error@-1 {{'B' cannot be constructed because it has no accessible initializers}}
      // expected-error@-2 {{cannot infer contextual base in reference to member 'min'}}
    }

    self.main::myNegate()

    main::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'main::fatalError' in scope}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C {}

extension C: @retroactive ModuleSelectorTestingKit::Equatable {
// FIXME improve: expected-error@-1 {{cannot find type 'ModuleSelectorTestingKit::Equatable' in scope}}

  @_implements(ModuleSelectorTestingKit::Equatable, ModuleSelectorTestingKit::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{53-79=}}
  // FIXME improve: expected-error@-2 {{cannot find type 'ModuleSelectorTestingKit::Equatable' in scope}}

  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool {
  // FIXME improve: expected-error@-1 {{cannot find type 'ModuleSelectorTestingKit::Bool' in scope}}

    ModuleSelectorTestingKit::fatalError() // no-error -- not typechecking function bodies
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())

  mutating func myNegate() {
  // FIXME improve: expected-note@-1 {{did you mean 'myNegate'?}}

    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
    // FIXME improve: expected-error@-1 3{{cannot find type 'ModuleSelectorTestingKit::Int' in scope}}
      (ModuleSelectorTestingKit::+)
      // FIXME: should fail????

    let magnitude: Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude
    // FIXME improve: expected-error@-1 {{'ModuleSelectorTestingKit::Magnitude' is not a member type of struct 'Swift.Int'}}

    _ = (fn, magnitude)

    if ModuleSelectorTestingKit::Bool.ModuleSelectorTestingKit::random() {
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::Bool' in scope}}

      ModuleSelectorTestingKit::negate()
      // expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::negate' in scope}}
    }
    else {
      self = ModuleSelectorTestingKit::C(value: .ModuleSelectorTestingKit::min)
      // FIXME improve: expected-error@-1 {{type 'Int' has no member 'ModuleSelectorTestingKit::min'}}

      self = C.ModuleSelectorTestingKit::init(value: .min)
    }

    self.ModuleSelectorTestingKit::myNegate()
    // FIXME improve: expected-error@-1 {{value of type 'C' has no member 'ModuleSelectorTestingKit::myNegate'}}

    ModuleSelectorTestingKit::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::fatalError' in scope}}
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`

extension Swift::D {}
// FIXME improve: expected-error@-1 {{cannot find type 'Swift::D' in scope}}

extension D: @retroactive Swift::Equatable {
// Caused by Swift::D failing to typecheck in `equals(_:_:)`: expected-error@-1 *{{extension outside of file declaring struct 'D' prevents automatic synthesis of '==' for protocol 'Equatable'}} expected-note@-1 *{{add stubs for conformance}}

  @_implements(Swift::Equatable, Swift::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{34-41=}}

  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool {
  // expected-error@-1 {{cannot find type 'Swift::D' in scope}}
  // expected-error@-2 {{cannot find type 'Swift::D' in scope}}
    Swift::fatalError() // no-error -- not typechecking function bodies
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: Swift::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'Swift::negate()' could not be found}}

  mutating func myNegate() {

    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
      (Swift::+)

    let magnitude: Int.Swift::Magnitude = Swift::magnitude
    // expected-error@-1 {{cannot find 'Swift::magnitude' in scope}}

    _ = (fn, magnitude)

    if Swift::Bool.Swift::random() {

      Swift::negate()
      // FIXME improve: expected-error@-1 {{cannot find 'Swift::negate' in scope}}
    }
    else {
      self = Swift::D(value: .Swift::min)
      // FIXME improve: expected-error@-1 {{cannot find 'Swift::D' in scope}}
      // expected-error@-2 {{cannot infer contextual base in reference to member 'Swift::min'}}

      self = D.Swift::init(value: .min)
      // FIXME improve: expected-error@-1 {{'D' cannot be constructed because it has no accessible initializers}}
      // expected-error@-2 {{cannot infer contextual base in reference to member 'min'}}
    }

    self.Swift::myNegate()
    // FIXME improve: expected-error@-1 {{value of type 'D' has no member 'Swift::myNegate'}}

    Swift::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

let mog: Never = fatalError()

func localVarsCantBeAccessedByModuleSelector() {
  let mag: Int.Swift::Magnitude = main::mag
  // expected-error@-1 {{cannot find 'main::mag' in scope}}

  let mog: Never = main::mog
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
  _ = Swift::print
  _ = Swift:: print
  _ = Swift ::print
  _ = Swift :: print

  _ = Swift::
  print

  _ = Swift
  ::print

  _ = Swift ::
  print

  _ = Swift
  :: print

  _ = Swift
  ::
  print
}

// Error cases

func main::decl1(
  // expected-error@-1 {{name in function declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{6-12=}}
  main::p1: main::A,
  // expected-error@-1 {{argument label cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{3-9=}}
  // FIXME: expected-error@-2 {{cannot find type 'main::A' in scope}}
  main::label p2: main::inout A,
  // expected-error@-1 {{argument label cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{3-9=}}
  // FIXME: expected-error@-2 {{expected identifier in dotted type}} should be something like {{type 'inout' is not imported through module 'main'}}
  label main::p3: @main::escaping () -> A
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{9-15=}}
  // FIXME: expected-error@-2 {{attribute can only be applied to declarations, not types}} should be something like {{type 'escaping' is not imported through module 'main'}}
  // FIXME: expected-error@-3 {{expected parameter type following ':'}}
) {
  let main::decl1a = "a"
  // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{7-13=}}

  var main::decl1b = "b"
  // expected-error@-1 {{name in variable declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{7-13=}}

  let (main::decl1c, main::decl1d) = ("c", "d")
  // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{8-14=}}
  // expected-error@-2 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-2 {{remove module selector from this name}} {{22-28=}}

  if let (main::decl1e, main::decl1f) = Optional(("e", "f")) {}
  // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{11-17=}}
  // expected-error@-2 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-2 {{remove module selector from this name}} {{25-31=}}

  guard let (main::decl1g, main::decl1h) = Optional(("g", "h")) else { return }
  // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{14-20=}}
  // expected-error@-2 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-2 {{remove module selector from this name}} {{28-34=}}

  switch Optional(main::decl1g) {
  // expected-error@-1 {{cannot find 'main::decl1g' in scope}}
  case Optional.some(let main::decl1i):
    // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{26-32=}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  // expected-error@-1 {{cannot find 'main::decl1g' in scope}}
  case let Optional.some(main::decl1j):
    // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{26-32=}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  // expected-error@-1 {{cannot find 'main::decl1g' in scope}}
 case let main::decl1k?:
    // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{11-17=}}
    break
  case .none:
    break
  }

  for main::decl1l in "lll" {}
  // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{7-13=}}

  "lll".forEach { [main::magnitude]
    // expected-error@-1 {{captured variable name cannot be qualified with a module selector}}
    // expected-note@-2 {{remove module selector from this name}} {{20-26=}}
    // expected-note@-3 {{explicitly capture into a variable named 'magnitude'}} {{20-20=magnitude = }}
    main::elem in print(elem)
    // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{5-11=}}
  }

  "lll".forEach { (main::elem) in print(elem) }
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{20-26=}}

  "lll".forEach { (main::elem) -> Void in print(elem) }
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{20-26=}}

  "lll".forEach { (main::elem: Character) -> Void in print(elem) }
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{20-26=}}
}
enum main::decl2 {
  // expected-error@-1 {{name in enum declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{6-12=}}

  case main::decl2a
  // expected-error@-1 {{name in enum 'case' declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{8-14=}}
}

struct main::decl3 {}
// expected-error@-1 {{name in struct declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{8-14=}}

class main::decl4<main::T> {}
// expected-error@-1 {{name in class declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{7-13=}}
// expected-error@-2 {{generic parameter name cannot be qualified with a module selector}} expected-note@-2 {{remove module selector from this name}} {{19-25=}}

typealias main::decl5 = main::Bool
// expected-error@-1 {{name in typealias declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{11-17=}}
// FIXME improve: expected-error@-2 {{cannot find type 'main::Bool' in scope}}

protocol main::decl6 {
  // expected-error@-1 {{name in protocol declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{10-16=}}

  associatedtype main::decl6a
  // expected-error@-1 {{name in associatedtype declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{18-24=}}
}

let main::decl7 = 7
// expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{5-11=}}

var main::decl8 = 8 {
// expected-error@-1 {{name in variable declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{5-11=}}

  willSet(main::newValue) {}
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{11-17=}}

  didSet(main::oldValue) {}
  // expected-error@-1 {{parameter name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{10-16=}}
}

struct Parent {
  func main::decl1() {}
  // expected-error@-1 {{name in function declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{8-14=}}

  enum main::decl2 {
  // expected-error@-1 {{name in enum declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{8-14=}}

    case main::decl2a
    // expected-error@-1 {{name in enum 'case' declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{10-16=}}
  }

  struct main::decl3 {}
  // expected-error@-1 {{name in struct declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{10-16=}}

  class main::decl4 {}
  // expected-error@-1 {{name in class declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{9-15=}}

  typealias main::decl5 = main::Bool
  // expected-error@-1 {{name in typealias declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{13-19=}}
  // FIXME improve: expected-error@-2 {{cannot find type 'main::Bool' in scope}}
}

@_swift_native_objc_runtime_base(main::BaseClass)
// expected-error@-1 {{attribute parameter value cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{34-40=}}
class C1 {}

infix operator <<<<< : Swift::AdditionPrecedence
// expected-error@-1 {{precedence group name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{24-31=}}

precedencegroup main::PG1 {
// expected-error@-1 {{precedence group name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{17-23=}}

  higherThan: Swift::AdditionPrecedence
  // expected-error@-1 {{precedence group name cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{15-22=}}
}

func badModuleNames() {
  NonexistentModule::print()
  // expected-error@-1 {{cannot find 'NonexistentModule::print' in scope}}

  _ = "foo".NonexistentModule::count
  // FIXME improve: expected-error@-1 {{value of type 'String' has no member 'NonexistentModule::count'}}
  // FIXME: expected-EVENTUALLY-note@-2 {{did you mean module 'Swift'?}} {{13-30=Swift}}

  let x: NonexistentModule::MyType = NonexistentModule::MyType()
  // expected-error@-1 {{cannot find type 'NonexistentModule::MyType' in scope}}

  let y: A.NonexistentModule::MyChildType = fatalError()
  // expected-error@-1 {{'NonexistentModule::MyChildType' is not a member type of struct 'ModuleSelectorTestingKit.A'}}
}

struct BadModuleSelectorSyntax { // expected-note {{in declaration of 'BadModuleSelectorSyntax'}}
  var a: ::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var b: (::Int)
  // expected-error@-1 {{expected identifier in module selector}}

  var c: *::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var d: _::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var e: Self::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var f: self::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var g: inout::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var h: Any::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var aArray: [::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var bArray: [(::Int)]
  // expected-error@-1 {{expected identifier in module selector}}

  var cArray: [*::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var dArray: [_::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var eArray: [Self::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var fArray: [self::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var gArray: [inout::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var hArray: [Any::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var aIndex: String.::Index
  // expected-error@-1 {{expected identifier in module selector}}

  // FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
  var cIndex: String.*::Index
  // expected-error@-1 {{consecutive declarations on a line must be separated by ';'}}
  // expected-error@-2 {{expected declaration}}

  var dIndex: String._::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var eIndex: String.Self::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var fIndex: String.self::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var gIndex: String.inout::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var hIndex: String.Any::Index
  // expected-error@-1 {{expected identifier in module selector}}

  func inExpr() {
    ::print()
    // expected-error@-1 {{expected identifier in module selector}}

    (::print())
    // expected-error@-1 {{expected identifier in module selector}}

    *::print()
    // expected-error@-1 {{expected identifier in module selector}}

    _::print()
    // expected-error@-1 {{expected identifier in module selector}}

    Self::print()
    // expected-error@-1 {{expected identifier in module selector}}

    self::print()
    // expected-error@-1 {{expected identifier in module selector}}

    inout::print()
    // expected-error@-1 {{expected identifier in module selector}}

    Any::print()
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = (1.::magnitude)
    // expected-error@-1 {{expected identifier in module selector}}

    // FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
    _ = 1.*::magnitude
    // expected-error@-1 {{expected identifier in module selector}}
    // expected-error@-2 {{cannot find operator '.*' in scope}}

    _ = 1._::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.Self::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.self::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.inout::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.Any::magnitude
    // expected-error@-1 {{expected identifier in module selector}}
  }
}

@_spi(main::Private)
// expected-error@-1 {{SPI group cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{7-13=}}
public struct BadImplementsAttr: CustomStringConvertible {
  @_implements(CustomStringConvertible, Swift::description)
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{41-48=}}
  public var stringValue: String { fatalError() }

  @_specialize(spi: main::Private, where T == Swift::Int)
  // expected-error@-1 {{SPI group cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{21-27=}}
  public func fn<T>() -> T { fatalError() }
}
