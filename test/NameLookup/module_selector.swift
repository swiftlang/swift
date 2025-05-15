// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-feature ModuleSelector
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-feature ModuleSelector -enable-experimental-feature ParserASTGen

// REQUIRES: swift_feature_ModuleSelector, swift_feature_ParserASTGen

// FIXME: This test doesn't really cover:
//
// * Whether X::foo finds foos in X's re-exports
// * Whether we handle access paths correctly
// * Interaction with ClangImporter
// * Cross-import overlays
// * Key path dynamic member lookup
// * Custom type attributes (and coverage of type attrs generally is sparse)
// * Macros
//
// It also might not cover all combinations of name lookup paths and inputs.

import ModuleSelectorTestingKit

import ctypes.bits
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

    _ = \ModuleSelectorTestingKit::A.magnitude
    _ = \A.ModuleSelectorTestingKit::magnitude
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of main:: using `B`

extension main::B {}
// FIXME improve: expected-error@-1 {{cannot find type 'main::B' in scope}}

extension B: @retroactive main::Equatable {
  // FIXME improve: expected-error@-1 {{cannot find type 'main::Equatable' in scope}}

  @_implements(main::Equatable, ==(_:_:))
  // FIXME improve: expected-error@-1 {{cannot find type 'main::Equatable' in scope}}

  public static func equals(_: main::B, _: main::B) -> main::Bool {
  // FIXME improve: expected-error@-1 {{cannot find type 'main::B' in scope}}
  // FIXME improve: expected-error@-2 {{cannot find type 'main::B' in scope}}
  // FIXME improve: expected-error@-3 {{cannot find type 'main::Bool' in scope}}
    main::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'main::fatalError' in scope}}
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'main::negate()' could not be found}}

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
    // FIXME improve: expected-error@-1 3{{cannot find type 'main::Int' in scope}}
      (main::+)
      // FIXME improve: expected-error@-1 {{cannot find operator 'main::+' in scope}}

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
    }

    self.main::myNegate()

    main::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'main::fatalError' in scope}}

    _ = \main::A.magnitude
    _ = \A.main::magnitude
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C {}

extension C: @retroactive ModuleSelectorTestingKit::Equatable {
// FIXME improve: expected-error@-1 {{cannot find type 'ModuleSelectorTestingKit::Equatable' in scope}}

  @_implements(ModuleSelectorTestingKit::Equatable, ==(_:_:))
  // FIXME improve: expected-error@-1 {{cannot find type 'ModuleSelectorTestingKit::Equatable' in scope}}

  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool {
  // FIXME improve: expected-error@-1 {{cannot find type 'ModuleSelectorTestingKit::Bool' in scope}}

    ModuleSelectorTestingKit::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::fatalError' in scope}}
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())

  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
    // FIXME improve: expected-error@-1 3{{cannot find type 'ModuleSelectorTestingKit::Int' in scope}}
      (ModuleSelectorTestingKit::+)
      // FIXME improve: expected-error@-1 {{cannot find operator 'ModuleSelectorTestingKit::+' in scope}}

    let magnitude: Int.ModuleSelectorTestingKit::Magnitude = ModuleSelectorTestingKit::magnitude
    // FIXME improve: expected-error@-1 {{'ModuleSelectorTestingKit::Magnitude' is not a member type of struct 'Swift.Int'}}
    // FIXME improve: expected-error@-2 {{cannot find 'ModuleSelectorTestingKit::magnitude' in scope}}

    _ = (fn, magnitude)

    if ModuleSelectorTestingKit::Bool.ModuleSelectorTestingKit::random() {
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::Bool' in scope}}

      ModuleSelectorTestingKit::negate()
      // expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::negate' in scope}}
    }
    else {
      self = ModuleSelectorTestingKit::C(value: .ModuleSelectorTestingKit::min)

      self = C.ModuleSelectorTestingKit::init(value: .min)
    }

    self.ModuleSelectorTestingKit::myNegate()

    ModuleSelectorTestingKit::fatalError()
    // FIXME improve: expected-error@-1 {{cannot find 'ModuleSelectorTestingKit::fatalError' in scope}}

    _ = \ModuleSelectorTestingKit::A.magnitude
    _ = \A.ModuleSelectorTestingKit::magnitude
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`

extension Swift::D {}
// FIXME improve: expected-error@-1 {{cannot find type 'Swift::D' in scope}}

extension D: @retroactive Swift::Equatable {
// Caused by Swift::D failing to typecheck in `equals(_:_:)`: expected-error@-1 *{{extension outside of file declaring struct 'D' prevents automatic synthesis of '==' for protocol 'Equatable'}} expected-note@-1 *{{add stubs for conformance}}

  @_implements(Swift::Equatable, ==(_:_:))
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
    }

    self.Swift::myNegate()

    Swift::fatalError()

    _ = \Swift::A.magnitude
    _ = \A.Swift::magnitude
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

// Error cases

func decl1(
  p1: main::A,
  // FIXME: expected-error@-1 {{cannot find type 'main::A' in scope}}
  label p2: inout A,
  label p3: @escaping () -> A
) {
  switch Optional(main::p2) {
  case Optional.some(let decl1i):
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
  case let Optional.some(decl1j):
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
 case let decl1k?:
    break
  case .none:
    break
  }
}

typealias decl5 = main::Bool
// FIXME improve: expected-error@-1 {{cannot find type 'main::Bool' in scope}}

func badModuleNames() {
  NonexistentModule::print()
  // expected-error@-1 {{cannot find 'NonexistentModule::print' in scope}}

  _ = "foo".NonexistentModule::count

  let x: NonexistentModule::MyType = NonexistentModule::MyType()
  // expected-error@-1 {{cannot find type 'NonexistentModule::MyType' in scope}}
  // expected-error@-2 {{cannot find 'NonexistentModule::MyType' in scope}}

  let y: A.NonexistentModule::MyChildType = fatalError()
  // expected-error@-1 {{'NonexistentModule::MyChildType' is not a member type of struct 'ModuleSelectorTestingKit.A'}}
}
