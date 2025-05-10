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

extension B: @retroactive main::Equatable {

  @_implements(main::Equatable, main::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{33-39=}}

  public static func equals(_: main::B, _: main::B) -> main::Bool {
    main::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
      (main::+)

    let magnitude: Int.main::Magnitude = main::magnitude

    _ = (fn, magnitude)

    if main::Bool.main::random() {

      main::negate()
    }
    else {
      self = main::B(value: .main::min)

      self = B.main::init(value: .min)
    }

    self.main::myNegate()

    main::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C {}

extension C: @retroactive ModuleSelectorTestingKit::Equatable {

  @_implements(ModuleSelectorTestingKit::Equatable, ModuleSelectorTestingKit::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{53-79=}}

  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool {
    ModuleSelectorTestingKit::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())

  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
      (ModuleSelectorTestingKit::+)

    let magnitude: Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude

    _ = (fn, magnitude)

    if ModuleSelectorTestingKit::Bool.ModuleSelectorTestingKit::random() {

      ModuleSelectorTestingKit::negate()
    }
    else {
      self = ModuleSelectorTestingKit::C(value: .ModuleSelectorTestingKit::min)

      self = C.ModuleSelectorTestingKit::init(value: .min)
    }

    self.ModuleSelectorTestingKit::myNegate()

    ModuleSelectorTestingKit::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`

extension Swift::D {}

extension D: @retroactive Swift::Equatable {

  @_implements(Swift::Equatable, Swift::==(_:_:))
  // expected-error@-1 {{name of sibling declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{34-41=}}

  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool {
    Swift::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: Swift::negate())

  mutating func myNegate() {

    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
      (Swift::+)

    let magnitude: Int.Swift::Magnitude = Swift::magnitude

    _ = (fn, magnitude)

    if Swift::Bool.Swift::random() {

      Swift::negate()
    }
    else {
      self = Swift::D(value: .Swift::min)

      self = D.Swift::init(value: .min)
    }

    self.Swift::myNegate()

    Swift::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}

let mog: Never = fatalError()

func localVarsCantBeAccessedByModuleSelector() {
  let mag: Int.Swift::Magnitude = main::mag
  // expected-error@-1 {{use of local variable 'main::mag' before its declaration}}
  // expected-note@-2 {{'mag' declared here}}

  let mog: Never = main::mog
}

struct AvailableUser {
  @available(macOS 10.15, *) var use1: String { "foo" }

  @main::available() var use2

  @ModuleSelectorTestingKit::available() var use4
  // no-error

  @Swift::available() var use5
}

func builderUser2(@main::MyBuilder fn: () -> Void) {}

func builderUser3(@ModuleSelectorTestingKit::MyBuilder fn: () -> Void) {}

func builderUser4(@Swift::MyBuilder fn: () -> Void) {}

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
  case Optional.some(let main::decl1i):
    // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{26-32=}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  case let Optional.some(main::decl1j):
    // expected-error@-1 {{name in constant declaration cannot be qualified with a module selector}} expected-note@-1 {{remove module selector from this name}} {{26-32=}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
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

  _ = "foo".NonexistentModule::count

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
