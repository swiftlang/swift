// RUN: %target-typecheck-verify-swift -module-name main -I %S/Inputs -enable-experimental-feature ModuleSelector

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -module-name main -I %S/Inputs 2>/dev/null

// REQUIRES: swift_feature_ModuleSelector

import ModuleSelectorTestingKit

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
    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
      (+)
      // FIXME: it'd be nice to handle module selectors on operators.

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
  // expected-error@-1 {{name cannot be qualified with module selector here}} {{33-39=}}

  public static func equals(_: main::B, _: main::B) -> main::Bool {
    main::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: main::negate())

  mutating func myNegate() {
    let fn: (main::Int, main::Int) -> main::Int =
      (main::+)
      // FIXME: it'd be nice to handle module selectors on operators.
      // expected-error@-2 {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}}
      // expected-error@-3 {{expected expression}}
      // expected-error@-4 {{expected expression after operator}}

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
  // expected-error@-1 {{name cannot be qualified with module selector here}} {{53-79=}}

  public static func equals(_: ModuleSelectorTestingKit::C, _: ModuleSelectorTestingKit::C) -> ModuleSelectorTestingKit::Bool {
    ModuleSelectorTestingKit::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())

  mutating func myNegate() {
    let fn: (ModuleSelectorTestingKit::Int, ModuleSelectorTestingKit::Int) -> ModuleSelectorTestingKit::Int =
      (ModuleSelectorTestingKit::+)
      // FIXME: it'd be nice to handle module selectors on operators.
      // expected-error@-2 {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}}
      // expected-error@-3 {{expected expression}}
      // expected-error@-4 {{expected expression after operator}}

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
  // expected-error@-1 {{name cannot be qualified with module selector here}} {{34-41=}}

  public static func equals(_: Swift::D, _: Swift::D) -> Swift::Bool {
    Swift::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: Swift::negate())

  mutating func myNegate() {
    let fn: (Swift::Int, Swift::Int) -> Swift::Int =
      (Swift::+)
      // FIXME: it'd be nice to handle module selectors on operators.
      // expected-error@-2 {{cannot convert value of type '()' to specified type '(Int, Int) -> Int'}}
      // expected-error@-3 {{expected expression}}
      // expected-error@-4 {{expected expression after operator}}

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
  // expected-error@-1 {{use of local variable 'mag' before its declaration}}
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

func badModuleNames() {
  NonexistentModule::print()

  _ = "foo".NonexistentModule::count

  let x: NonexistentModule::MyType = NonexistentModule::MyType()
  // expected-error@-1 {{cannot find type 'MyType' in scope}}

  let y: A.NonexistentModule::MyChildType = fatalError()
  // expected-error@-1 {{'MyChildType' is not a member type of struct 'ModuleSelectorTestingKit.A'}}
}
