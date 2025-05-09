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

extension B: @retroactive main::Equatable {

  @_implements(main::Equatable, ==(_:_:))

  public static func equals(_: main::B, _: main::B) -> main::Bool {
    main::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'main::negate()' could not be found}}

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
      (main::+)

    let magnitude: Int.main::Magnitude = main::magnitude
    // FIXME improve: expected-error@-1 {{'main::Magnitude' is not a member type of struct 'Swift.Int'}}

    _ = (fn, magnitude)

    if main::Bool.main::random() {

      main::negate()
      // FIXME improve: expected-error@-1 {{cannot find 'main::negate' in scope}}
    }
    else {
      self = main::B(value: .main::min)

      self = B.main::init(value: .min)
    }

    self.main::myNegate()

    main::fatalError()

    _ = \main::A.magnitude
    _ = \A.main::magnitude
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of ModuleSelectorTestingKit:: using `C`

extension ModuleSelectorTestingKit::C {}

extension C: @retroactive ModuleSelectorTestingKit::Equatable {

  @_implements(ModuleSelectorTestingKit::Equatable, ==(_:_:))

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
    // FIXME improve: expected-error@-1 {{'ModuleSelectorTestingKit::Magnitude' is not a member type of struct 'Swift.Int'}}

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

    _ = \ModuleSelectorTestingKit::A.magnitude
    _ = \A.ModuleSelectorTestingKit::magnitude
  }

  // FIXME: Can we test @convention(witness_method:)?
}

// Test resolution of Swift:: using `D`

extension Swift::D {}

extension D: @retroactive Swift::Equatable {

  @_implements(Swift::Equatable, ==(_:_:))
  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool {
    Swift::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: Swift::negate())
  // FIXME improve: expected-error@-1 {{replaced function 'Swift::negate()' could not be found}}

  mutating func myNegate() {

    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
      (Swift::+)

    let magnitude: Int.Swift::Magnitude = Swift::magnitude
    // expected-error@-1 {{cannot convert value of type 'Never' to specified type 'Int.Magnitude' (aka 'UInt')}}

    _ = (fn, magnitude)

    if Swift::Bool.Swift::random() {

      Swift::negate()
      // FIXME improve: expected-error@-1 {{cannot find 'Swift::negate' in scope}}
    }
    else {
      self = Swift::D(value: .Swift::min)

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

// Error cases

func decl1(
  p1: main::A,
  label p2: inout A,
  label p3: @escaping () -> A
) {
  switch Optional(main::p2) {
  case Optional.some(let decl1i):
    // expected-warning@-1 {{immutable value 'decl1i' was never used; consider replacing with '_' or removing it}}
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
  case let Optional.some(decl1j):
    // expected-warning@-1 {{immutable value 'decl1j' was never used; consider replacing with '_' or removing it}}
    break
  case .none:
    break
  }

  switch Optional(main::p2) {
 case let decl1k?:
    // expected-warning@-1 {{immutable value 'decl1k' was never used; consider replacing with '_' or removing it}}
    break
  case .none:
    break
  }
}

typealias decl5 = main::Bool

func badModuleNames() {
  NonexistentModule::print()

  _ = "foo".NonexistentModule::count

  let x: NonexistentModule::MyType = NonexistentModule::MyType()
  // expected-error@-1 {{cannot find type 'NonexistentModule::MyType' in scope}}
  // expected-error@-2 {{cannot find 'NonexistentModule::MyType' in scope}}

  let y: A.NonexistentModule::MyChildType = fatalError()
  // expected-error@-1 {{'NonexistentModule::MyChildType' is not a member type of struct 'ModuleSelectorTestingKit.A'}}
}
