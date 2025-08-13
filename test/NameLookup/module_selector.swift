// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-builtin-module -enable-experimental-feature ModuleSelector
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-builtin-module -enable-experimental-feature ModuleSelector -enable-experimental-feature ParserASTGen

// REQUIRES: swift_feature_ModuleSelector, swift_feature_ParserASTGen

// FIXME: This test doesn't really cover:
//
// * Whether X::foo finds foos in X's re-exports
// * Whether we handle access paths correctly
// * Interaction with ClangImporter
// * Cross-import overlays
// * Key path dynamic member lookup
// * Custom type attributes (and coverage of type attrs generally is sparse)
//
// It also might not cover all combinations of name lookup paths and inputs.

import ModuleSelectorTestingKit

import ctypes.bits
import struct ModuleSelectorTestingKit::A

import Builtin

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

    _ = \ModuleSelectorTestingKit::A.magnitude
    _ = \A.ModuleSelectorTestingKit::magnitude

    _ = #ModuleSelectorTestingKit::ExprMacro
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of main:: using `B`

extension main::B {}
// expected-error@-1 {{'B' is not imported through module 'main'}}
// expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{11-15=ModuleSelectorTestingKit}}

extension B: @retroactive main::Equatable {
  // expected-error@-1 {{'Equatable' is not imported through module 'main'}}
  // expected-note@-2 {{did you mean module 'Swift'?}} {{27-31=Swift}}

  @_implements(main::Equatable, ==(_:_:))
  // expected-error@-1 {{'Equatable' is not imported through module 'main'}}
  // expected-note@-2 {{did you mean module 'Swift'?}} {{16-20=Swift}}

  public static func equals(_: main::B, _: main::B) -> main::Bool {
    // expected-error@-1 2{{'B' is not imported through module 'main'}}
    // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{32-36=ModuleSelectorTestingKit}}
    // expected-note@-3 {{did you mean module 'ModuleSelectorTestingKit'?}} {{44-48=ModuleSelectorTestingKit}}
    // expected-error@-4 {{'Bool' is not imported through module 'main'}}
    // expected-note@-5 {{did you mean module 'Swift'?}} {{56-60=Swift}}
    main::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'main::fatalError' in scope}}
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'main::negate()' could not be found}}

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
    // expected-error@-1 3{{'Int' is not imported through module 'main'}}
    // expected-note@-2 {{did you mean module 'Swift'?}} {{14-18=Swift}}
    // expected-note@-3 {{did you mean module 'Swift'?}} {{25-29=Swift}}
    // expected-note@-4 {{did you mean module 'Swift'?}} {{39-43=Swift}}
      (main::+)
      // FIXME improve: expected-error@-1 {{cannot find operator 'main::+' in scope}}

    let magnitude: Int.main::Magnitude = main::magnitude
    // expected-error@-1 {{'Magnitude' is not imported through module 'main'}}
    // expected-note@-2 {{did you mean module 'Swift'?}} {{24-28=Swift}}

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

    _ = \main::A.magnitude
    // expected-error@-1 {{'A' is not imported through module 'main'}}
    // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{10-14=ModuleSelectorTestingKit}}
    _ = \A.main::magnitude
    // FIXME improve: expected-error@-1 {{value of type 'A' has no member 'main::magnitude'}}

    _ = #main::ExprMacro
    // expected-error@-1 {{no macro named 'main::ExprMacro'}}
  }

  @main::PeerMacro func thingy() {}
  // expected-error@-1 {{unknown attribute 'main::PeerMacro'}}

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C {}

extension C: @retroactive ModuleSelectorTestingKit::Equatable {
  // expected-error@-1 {{'Equatable' is not imported through module 'ModuleSelectorTestingKit'}}
  // expected-note@-2 {{did you mean module 'Swift'?}} {{27-51=Swift}}

  @_implements(ModuleSelectorTestingKit::Equatable, ==(_:_:))
  // expected-error@-1 {{'Equatable' is not imported through module 'ModuleSelectorTestingKit'}}
  // expected-note@-2 {{did you mean module 'Swift'?}} {{16-40=Swift}}

  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool {
    // expected-error@-1 {{'Bool' is not imported through module 'ModuleSelectorTestingKit'}}
    // expected-note@-2 {{did you mean module 'Swift'?}} {{96-120=Swift}}

    ModuleSelectorTestingKit::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::fatalError' in scope}}
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())

  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
    // expected-error@-1 3{{'Int' is not imported through module 'ModuleSelectorTestingKit'}}
    // expected-note@-2 {{did you mean module 'Swift'?}} {{14-38=Swift}}
    // expected-note@-3 {{did you mean module 'Swift'?}} {{45-69=Swift}}
    // expected-note@-4 {{did you mean module 'Swift'?}} {{79-103=Swift}}
      (ModuleSelectorTestingKit::+)
      // FIXME improve: expected-error@-1 {{cannot find operator 'ModuleSelectorTestingKit::+' in scope}}

    let magnitude: Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude
    // expected-error@-1 {{'Magnitude' is not imported through module 'ModuleSelectorTestingKit'}}
    // expected-note@-2 {{did you mean module 'Swift'?}} {{24-48=Swift}}
    // FIXME improve: expected-error@-3 {{cannot find 'ModuleSelectorTestingKit::magnitude' in scope}}

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

    _ = \ModuleSelectorTestingKit::A.magnitude
    _ = \A.ModuleSelectorTestingKit::magnitude

    _ = #ModuleSelectorTestingKit::ExprMacro
  }

  @ModuleSelectorTestingKit::PeerMacro func thingy() {}
  // expected-error@-1 {{external macro implementation type 'Fnord.PeerMacro' could not be found for macro 'PeerMacro()'; plugin for module 'Fnord' not found}}

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`

extension Swift::D {}
// expected-error@-1 {{'D' is not imported through module 'Swift'}}
// expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{11-16=ModuleSelectorTestingKit}}

extension D: @retroactive Swift::Equatable {
// Caused by Swift::D failing to typecheck in `equals(_:_:)`: expected-error@-1 *{{extension outside of file declaring struct 'D' prevents automatic synthesis of '==' for protocol 'Equatable'}} expected-note@-1 *{{add stubs for conformance}}

  @_implements(Swift::Equatable, ==(_:_:))
  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool {
  // expected-error@-1 2{{'D' is not imported through module 'Swift'}}
  // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{32-37=ModuleSelectorTestingKit}}
  // expected-note@-3 {{did you mean module 'ModuleSelectorTestingKit'?}} {{45-50=ModuleSelectorTestingKit}}
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

    _ = \Swift::A.magnitude
    // expected-error@-1 {{'A' is not imported through module 'Swift'}}
    // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{10-15=ModuleSelectorTestingKit}}
    _ = \A.Swift::magnitude
    // FIXME improve: expected-error@-1 {{value of type 'A' has no member 'Swift::magnitude'}}

    _ = #Swift::ExprMacro
    // expected-error@-1 {{no macro named 'Swift::ExprMacro'}}
  }

  @Swift::PeerMacro func thingy() {}
  // expected-error@-1 {{unknown attribute 'Swift::PeerMacro'}}

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
  // FIXME improve: expected-error@-1 {{unknown attribute 'main::available'}}
  // FIXME suppress: expected-error@-2 {{type annotation missing in pattern}}

  @ModuleSelectorTestingKit::available() var use4
  // no-error

  @Swift::available() var use5
  // FIXME improve: expected-error@-1 {{unknown attribute 'Swift::available'}}
  // FIXME suppress: expected-error@-2 {{type annotation missing in pattern}}
}

func builderUser2(@main::MyBuilder fn: () -> Void) {}
// FIXME improve: expected-error@-1 {{unknown attribute 'main::MyBuilder'}}

func builderUser3(@ModuleSelectorTestingKit::MyBuilder fn: () -> Void) {}
// no-error

func builderUser4(@Swift::MyBuilder fn: () -> Void) {}
// FIXME improve: expected-error@-1 {{unknown attribute 'Swift::MyBuilder'}}

// Error cases

func decl1(
  p1: main::A,
  // expected-error@-1 {{'A' is not imported through module 'main'}}
  // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{7-11=ModuleSelectorTestingKit}}
  label p2: inout A,
  label p3: @escaping () -> A
) {
  switch Optional(main::p2) {
  // expected-error@-1 {{cannot find 'main::p2' in scope}}
  case Optional.some(let decl1i):
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
  // expected-error@-1 {{cannot find 'main::p2' in scope}}
  case let Optional.some(decl1j):
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
  // expected-error@-1 {{cannot find 'main::p2' in scope}}
 case let decl1k?:
    break
  case .none:
    break
  }
}

typealias decl5 = main::Bool
// expected-error@-1 {{'Bool' is not imported through module 'main'}}
// expected-note@-2 {{did you mean module 'Swift'?}} {{19-23=Swift}}

func badModuleNames() {
  NonexistentModule::print()
  // expected-error@-1 {{cannot find 'NonexistentModule::print' in scope}}

  _ = "foo".NonexistentModule::count
  // FIXME improve: expected-error@-1 {{value of type 'String' has no member 'NonexistentModule::count'}}
  // FIXME: expected-EVENTUALLY-note@-2 {{did you mean module 'Swift'?}} {{13-30=Swift}}

  let x: NonexistentModule::MyType = NonexistentModule::MyType()
  // expected-error@-1 {{cannot find type 'NonexistentModule::MyType' in scope}}
  // expected-error@-2 {{cannot find 'NonexistentModule::MyType' in scope}}

  let y: A.NonexistentModule::MyChildType = fatalError()
  // expected-error@-1 {{'NonexistentModule::MyChildType' is not a member type of struct 'ModuleSelectorTestingKit.A'}}
}

func builtinModuleLookups(_ int: Builtin::Int64) -> Builtin::Int64 {
  return Builtin::int_bswap_Int64(int)
}
