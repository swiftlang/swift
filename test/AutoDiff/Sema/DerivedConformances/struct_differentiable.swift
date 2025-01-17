// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_differentiable_other_module.swift

import _Differentiation

// Verify that a type `T` conforms to `AdditiveArithmetic`.
func assertConformsToAdditiveArithmetic<T>(_: T.Type)
where T: AdditiveArithmetic {}

struct Empty: Differentiable {}
func testEmpty() {
  assertConformsToAdditiveArithmetic(Empty.TangentVector.self)
}

struct EmptyWithConcreteNonmutatingMoveAlong: Differentiable {
  typealias TangentVector = Empty.TangentVector
  func move(by _: TangentVector) {}
  static func proof_that_i_have_nonmutating_move_along() {
    let empty = Self()
    empty.move(by: .init())
  }
}

protocol DifferentiableWithNonmutatingMoveAlong: Differentiable {}
extension DifferentiableWithNonmutatingMoveAlong {
  func move(by _: TangentVector) {}
}

struct EmptyWithInheritedNonmutatingMoveAlong: DifferentiableWithNonmutatingMoveAlong {
  typealias TangentVector = Empty.TangentVector
  static func proof_that_i_have_nonmutating_move_along() {
    let empty = Self()
    empty.move(by: .init())
  }
}

class EmptyClass: Differentiable {}
func testEmptyClass() {
  assertConformsToAdditiveArithmetic(EmptyClass.TangentVector.self)
}

// Test interaction with `AdditiveArithmetic` derived conformances.
// Previously, this crashed due to duplicate memberwise initializer synthesis.
struct EmptyAdditiveArithmetic: AdditiveArithmetic, Differentiable {}

// Test structs with `let` stored properties.
// Derived conformances fail because `mutating func move` requires all stored
// properties to be mutable.
struct ImmutableStoredProperties: Differentiable {
  var okay: Float

  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Int' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let nondiff: Int

  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'ImmutableStoredProperties' requires all stored properties not marked with '@noDerivative' to be mutable or have a non-mutating 'move(by:)'; use 'var' instead, or add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let diff: Float

  let nonmutatingMoveAlongStruct: EmptyWithConcreteNonmutatingMoveAlong

  let inheritedNonmutatingMoveAlongStruct: EmptyWithInheritedNonmutatingMoveAlong
  
  let diffClass: EmptyClass // No error on class-bound `let` with a non-mutating `move(by:)`.
}
func testImmutableStoredProperties() {
  _ = ImmutableStoredProperties.TangentVector(
    okay: 1, 
    nonmutatingMoveAlongStruct: Empty.TangentVector(), 
    inheritedNonmutatingMoveAlongStruct: Empty.TangentVector(),
    diffClass: EmptyClass.TangentVector())
}
struct MutableStoredPropertiesWithInitialValue: Differentiable {
  var x = Float(1)
  var y = Double(1)
}
// Test struct with both an empty constructor and memberwise initializer.
struct AllMixedStoredPropertiesHaveInitialValue: Differentiable {
  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'AllMixedStoredPropertiesHaveInitialValue' requires all stored properties not marked with '@noDerivative' to be mutable or have a non-mutating 'move(by:)'; use 'var' instead, or add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let x = Float(1)
  var y = Float(1)
  // Memberwise initializer should be `init(y:)` since `x` is immutable.
  static func testMemberwiseInitializer() {
    _ = AllMixedStoredPropertiesHaveInitialValue(y: 1)
  }
}
struct HasCustomConstructor: Differentiable {
  var x = Float(1)
  var y = Float(1)
  // Custom constructor should not affect synthesis.
  init(x: Float, y: Float, z: Bool) {}
}

struct Simple: AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
}
func testSimple() {
  var simple = Simple(w: 1, b: 1)
  simple.move(by: simple)
}

// Test type with mixed members.
struct Mixed: AdditiveArithmetic, Differentiable {
  var simple: Simple
  var float: Float
}
func testMixed(_ simple: Simple) {
  var mixed = Mixed(simple: simple, float: 1)
  mixed.move(by: mixed)
}

// Test type with manual definition of vector space types to `Self`.
struct VectorSpacesEqualSelf: AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = VectorSpacesEqualSelf
}

// Test generic type with vector space types to `Self`.
struct GenericVectorSpacesEqualSelf<T>: AdditiveArithmetic, Differentiable
where T: Differentiable, T == T.TangentVector {
  var w: T
  var b: T
}
func testGenericVectorSpacesEqualSelf() {
  var genericSame = GenericVectorSpacesEqualSelf<Double>(w: 1, b: 1)
  genericSame.move(by: genericSame)
}

// Test nested type.
struct Nested: AdditiveArithmetic, Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>
}
func testNested(
  _ simple: Simple, _ mixed: Mixed,
  _ genericSame: GenericVectorSpacesEqualSelf<Double>
) {
  var nested = Nested(simple: simple, mixed: mixed, generic: genericSame)
  nested.move(by: nested)
}

// Test type that does not conform to `AdditiveArithmetic` but whose members do.
// Thus, `Self` cannot be used as `TangentVector` or `TangentVector`.
// Vector space structs types must be synthesized.
// Note: it would be nice to emit a warning if conforming `Self` to
// `AdditiveArithmetic` is possible.
struct AllMembersAdditiveArithmetic: Differentiable {
  var w: Float
  var b: Float
}

// Test type whose properties are not all differentiable.
struct DifferentiableSubset: Differentiable {
  var w: Float
  var b: Float
  @noDerivative var flag: Bool
  @noDerivative let technicallyDifferentiable: Float = .pi
}
func testDifferentiableSubset() {
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
}

// Test nested type whose properties are not all differentiable.
struct NestedDifferentiableSubset: Differentiable {
  var x: DifferentiableSubset
  var mixed: Mixed
  @noDerivative var technicallyDifferentiable: Float
}

// Test type that uses synthesized vector space types but provides custom
// method.
struct HasCustomMethod: Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>
  mutating func move(by offset: TangentVector) {
    print("Hello world")
    simple.move(by: offset.simple)
    mixed.move(by: offset.mixed)
    generic.move(by: offset.generic)
  }
}

// Test type with user-defined memberwise initializer.
struct TF_25: Differentiable {
  public var bar: Float
  public init(bar: Float) {
    self.bar = bar
  }
}
// Test user-defined memberwise initializer.
struct TF_25_Generic<T: Differentiable>: Differentiable {
  public var bar: T
  public init(bar: T) {
    self.bar = bar
  }
}

// Test initializer that is not a memberwise initializer because of stored property name vs parameter label mismatch.
struct HasCustomNonMemberwiseInitializer<T: Differentiable>: Differentiable {
  var value: T
  init(randomLabel value: T) { self.value = value }
}

// Test type with generic environment.
struct HasGenericEnvironment<Scalar: FloatingPoint & Differentiable>: Differentiable
{
  var x: Float
}

// Test type with generic members that conform to `Differentiable`.
struct GenericSynthesizeAllStructs<T: Differentiable>: Differentiable {
  var w: T
  var b: T
}

// Test type in generic context.
struct A<T: Differentiable> {
  struct B<U: Differentiable, V>: Differentiable {
    struct InGenericContext: Differentiable {
      @noDerivative var a: A
      var b: B
      var t: T
      var u: U
    }
  }
}

// Test extension.
struct Extended {
  var x: Float
}
extension Extended: Differentiable {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended: Differentiable where T: Differentiable {}

// Test constrained extension of generic type.
struct GenericConstrained<T> {
  var x: T
}
extension GenericConstrained: Differentiable
where T: Differentiable {}

struct TF_260<T: Differentiable>: Differentiable, AdditiveArithmetic {
  var x: T.TangentVector
}

// TF-269: Test crash when differentiation properties have no getter.
// Related to access levels and associated type inference.
public protocol TF_269_Layer: Differentiable {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  func applied(to input: Input) -> Output
}

public struct TF_269: TF_269_Layer {
  public var filter: Float
  public typealias Activation = @differentiable(reverse) (Output) -> Output
  @noDerivative public let activation: Activation

  public func applied(to input: Float) -> Float {
    return input
  }
}

// Test errors.

// Test manually customizing vector space types.
// These should fail. Synthesis is semantically unsupported if vector space
// types are customized.
// expected-error @+1 {{type 'VectorSpaceTypeAlias' does not conform to protocol 'Differentiable'}}
struct VectorSpaceTypeAlias: AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = Simple
}
// expected-error @+1 {{type 'VectorSpaceCustomStruct' does not conform to protocol 'Differentiable'}}
struct VectorSpaceCustomStruct: AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  struct TangentVector: AdditiveArithmetic, Differentiable {
    var w: Float.TangentVector
    var b: Float.TangentVector
    typealias TangentVector = VectorSpaceCustomStruct.TangentVector
  }
}

struct StaticNoDerivative: Differentiable {
  @noDerivative static var s: Bool = true
}

struct StaticMembersShouldNotAffectAnything: AdditiveArithmetic, Differentiable
{
  static var x: Bool = true
  static var y: Bool = false
}

struct ImplicitNoDerivative: Differentiable {
  var a: Float
  var b: Bool  // expected-warning {{stored property 'b' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
}

struct ImplicitNoDerivativeWithSeparateTangent: Differentiable {
  var x: DifferentiableSubset
  var b: Bool  // expected-warning {{stored property 'b' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
}

// TF-1018: verify that `@noDerivative` warnings are always silenceable, even
// when the `Differentiable` conformance context is not the nominal type
// declaration.

struct ExtensionDifferentiableNoDerivative<T> {
  // expected-warning @+2 {{stored property 'x' has no derivative because 'T' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  // expected-warning @+1 {{stored property 'y' has no derivative because 'T' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var x, y: T
  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var nondiff: Bool
}
extension ExtensionDifferentiableNoDerivative: Differentiable {}

struct ExtensionDifferentiableNoDerivativeFixed<T> {
  @noDerivative var x, y: T
  @noDerivative var nondiff: Bool
}
extension ExtensionDifferentiableNoDerivativeFixed: Differentiable {}

struct ConditionalDifferentiableNoDerivative<T> {
  var x, y: T
  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var nondiff: Bool
}
extension ConditionalDifferentiableNoDerivative: Differentiable
where T: Differentiable {}

struct ConditionalDifferentiableNoDerivativeFixed<T> {
  var x, y: T
  @noDerivative var nondiff: Bool
}
extension ConditionalDifferentiableNoDerivativeFixed: Differentiable
where T: Differentiable {}

// TF-265: Test invalid initializer (that uses a non-existent type).
struct InvalidInitializer: Differentiable {
  init(filterShape: (Int, Int, Int, Int), blah: NonExistentType) {}  // expected-error {{cannot find type 'NonExistentType' in scope}}
}

// Test memberwise initializer synthesis.
struct NoMemberwiseInitializerExtended<T> {
  var value: T
  init(_ value: T) {
    self.value = value
  }
}
extension NoMemberwiseInitializerExtended: Equatable, AdditiveArithmetic
where T: AdditiveArithmetic {}
extension NoMemberwiseInitializerExtended: Differentiable
where T: Differentiable & AdditiveArithmetic {}

// https://github.com/apple/swift/issues/55238
// Test interaction with `@differentiable` and `@derivative` type-checking.

struct S_55238: Differentiable {
  var x: Float

  @differentiable(reverse)
  func method() -> Float { x }

  @derivative(of: method)
  func vjpMethod() -> (value: Float, pullback: (Float) -> TangentVector) { fatalError() }

  // Test usage of synthesized `TangentVector` type.
  // This should not produce an error: "reference to invalid associated type 'TangentVector'".
  mutating func move(by offset: TangentVector) {}
}

// Test property wrappers.

@propertyWrapper
struct ImmutableWrapper<Value> {
  private var value: Value
  var wrappedValue: Value { value }
}

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

@propertyWrapper
class ClassWrapper<Value> {
  var wrappedValue: Value
  init(wrappedValue: Value) { self.wrappedValue = wrappedValue }
}

struct Generic<T> {}
extension Generic: Differentiable where T: Differentiable {}

struct WrappedProperties: Differentiable {
  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'WrappedProperties' requires 'wrappedValue' in property wrapper 'ImmutableWrapper' to be mutable or have a non-mutating 'move(by:)'; add an explicit '@noDerivative' attribute}}
  @ImmutableWrapper var immutableInt: Generic<Int>

  // expected-warning @+1 {{stored property 'mutableInt' has no derivative because 'Generic<Int>' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  @Wrapper var mutableInt: Generic<Int>

  @Wrapper var float: Generic<Float>
  @ClassWrapper var float2: Generic<Float>

  // https://github.com/apple/swift/issues/55517
  // Test `@differentiable` wrapped property.
  @differentiable(reverse) @Wrapper var float3: Generic<Float>

  @noDerivative @ImmutableWrapper var nondiff: Generic<Int>

  static func testTangentMemberwiseInitializer() {
    _ = TangentVector(float: .init(), float2: .init(), float3: .init())
  }
}

// Verify that cross-file derived conformances are disallowed.

extension OtherFileNonconforming: Differentiable {}
// expected-error@-1 {{extension outside of file declaring struct 'OtherFileNonconforming' prevents automatic synthesis of 'move(by:)' for protocol 'Differentiable'}}
// expected-note@-2 {{add stubs for conformance}}

extension GenericOtherFileNonconforming: Differentiable {}
// expected-error@-1 {{extension outside of file declaring generic struct 'GenericOtherFileNonconforming' prevents automatic synthesis of 'move(by:)' for protocol 'Differentiable'}}
// expected-note@-2 {{add stubs for conformance}}
