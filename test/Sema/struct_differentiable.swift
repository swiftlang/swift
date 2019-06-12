// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_differentiable_other_module.swift

// Verify that a `Differentiable` type upholds `AllDifferentiableVariables == TangentVector`.
func assertAllDifferentiableVariablesEqualsTangentVector<T>(_: T.Type)
  where T : Differentiable, T.AllDifferentiableVariables == T.TangentVector {}

// Verify that a type `T` conforms to `AdditiveArithmetic`.
func assertConformsToAdditiveArithmetic<T>(_: T.Type) where T : AdditiveArithmetic {}

// Verify that a type `T` conforms to `VectorProtocol`.
func assertConformsToVectorProtocol<T>(_: T.Type) where T : VectorProtocol {}

struct Empty : Differentiable {}
func testEmpty() {
  assertConformsToAdditiveArithmetic(Empty.AllDifferentiableVariables.self)
}

// Test interaction with `AdditiveArithmetic` derived conformances.
// Previously, this crashed due to duplicate memberwise initializer synthesis.
struct EmptyAdditiveArithmetic : AdditiveArithmetic, Differentiable {}

// Test structs whose stored properties all have a default value.
struct AllLetStoredPropertiesHaveInitialValue : Differentiable {
  // expected-warning @+1 {{'let' properties with a default value do not have a derivative; add '@noDerivative' to make it explicit, or change it to 'var' to allow derivatives}} {{3-3=@noDerivative }}
  let x = Float(1)
  // expected-warning @+1 {{'let' properties with a default value do not have a derivative; add '@noDerivative' to make it explicit, or change it to 'var' to allow derivatives}} {{3-3=@noDerivative }}
  let y = Float(1)
}
struct AllVarStoredPropertiesHaveInitialValue : Differentiable {
  var x = Float(1)
  var y = Float(1)
}
// Test struct with both an empty constructor and memberwise initializer.
struct AllMixedStoredPropertiesHaveInitialValue : Differentiable {
  let x = Float(1) // expected-warning {{'let' properties with a default value do not have a derivative}} {{3-3=@noDerivative }}
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

struct Simple : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
}
func testSimple() {
  var simple = Simple(w: 1, b: 1)
  simple.allDifferentiableVariables = simple + simple
  assert(simple.moved(along: simple) == simple + simple)
}

// Test type with mixed members.
struct Mixed : AdditiveArithmetic, Differentiable {
  var simple: Simple
  var float: Float
}
func testMixed(_ simple: Simple) {
  var mixed = Mixed(simple: simple, float: 1)
  mixed.allDifferentiableVariables = Mixed(simple: simple, float: 2)
  assert(mixed.moved(along: mixed) == mixed + mixed)
}

// Test type with manual definition of vector space types to `Self`.
struct VectorSpacesEqualSelf : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = VectorSpacesEqualSelf
  typealias AllDifferentiableVariables = VectorSpacesEqualSelf
}

// Test generic type with vector space types to `Self`.
struct GenericVectorSpacesEqualSelf<T> : AdditiveArithmetic, Differentiable
  where T : Differentiable, T == T.TangentVector,
        T == T.AllDifferentiableVariables
{
  var w: T
  var b: T
}
func testGenericVectorSpacesEqualSelf() {
  var genericSame = GenericVectorSpacesEqualSelf<Double>(w: 1, b: 1)
  genericSame.allDifferentiableVariables = genericSame + genericSame
  assert(genericSame.moved(along: genericSame) == genericSame + genericSame)
}

// Test nested type.
struct Nested : AdditiveArithmetic, Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>
}
func testNested(
  _ simple: Simple, _ mixed: Mixed,
  _ genericSame: GenericVectorSpacesEqualSelf<Double>
) {
  let nested = Nested(simple: simple, mixed: mixed, generic: genericSame)
  assert(nested.moved(along: nested) == nested + nested)

  _ = pullback(at: nested) { model in
    model.simple + model.simple
  }
}

// Test type that does not conform to `AdditiveArithmetic` but whose members do.
// Thus, `Self` cannot be used as `TangentVector` or `TangentVector`.
// Vector space structs types must be synthesized.
// Note: it would be nice to emit a warning if conforming `Self` to
// `AdditiveArithmetic` is possible.
struct AllMembersAdditiveArithmetic : Differentiable {
  var w: Float
  var b: Float
}
func testAllMembersAdditiveArithmetic() {
  assertAllDifferentiableVariablesEqualsTangentVector(AllMembersAdditiveArithmetic.self)
}

// Test type `AllMembersVectorProtocol` whose members conforms to `VectorProtocol`,
// in which case we should make `TangentVector` and `TangentVector` conform to
// `VectorProtocol`.
struct MyVector : VectorProtocol, Differentiable {
  var w: Float
  var b: Float
}
struct AllMembersVectorProtocol : Differentiable {
  var w: MyVector
  var b: MyVector
}
func testAllMembersVectorProtocol() {
  assertConformsToVectorProtocol(AllMembersVectorProtocol.TangentVector.self)
  assertConformsToVectorProtocol(AllMembersVectorProtocol.TangentVector.self)
}

// Test type with immutable, differentiable stored property.
struct ImmutableStoredProperty : Differentiable {
  var w: Float
  let fixedBias: Float = .pi // expected-warning {{'let' properties with a default value do not have a derivative}} {{3-3=@noDerivative }}
}
func testImmutableStoredProperty() {
  _ = ImmutableStoredProperty.TangentVector(w: 1)
}

// Test type whose properties are not all differentiable.
struct DifferentiableSubset : Differentiable {
  var w: Float
  var b: Float
  @noDerivative var flag: Bool
  @noDerivative let technicallyDifferentiable: Float = .pi
}
func testDifferentiableSubset() {
  assertConformsToAdditiveArithmetic(DifferentiableSubset.AllDifferentiableVariables.self)
  assertConformsToVectorProtocol(DifferentiableSubset.AllDifferentiableVariables.self)
  assertAllDifferentiableVariablesEqualsTangentVector(DifferentiableSubset.self)
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
  _ = DifferentiableSubset.AllDifferentiableVariables(w: 1, b: 1)

  _ = pullback(at: DifferentiableSubset(w: 1, b: 2, flag: false)) { model in
    model.w + model.b
  }
}

// Test nested type whose properties are not all differentiable.
struct NestedDifferentiableSubset : Differentiable {
  var x: DifferentiableSubset
  var mixed: Mixed
  @noDerivative var technicallyDifferentiable: Float
}
func testNestedDifferentiableSubset() {
  assertAllDifferentiableVariablesEqualsTangentVector(NestedDifferentiableSubset.self)
}

// Test type that uses synthesized vector space types but provides custom
// method.
struct HasCustomMethod : Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>
  func moved(along: TangentVector) -> HasCustomMethod {
     print("Hello world")
     return self
  }
}

// Test type that conforms to `KeyPathIterable`.
// The `AllDifferentiableVariables` struct should also conform to `KeyPathIterable`.
struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative let technicallyDifferentiable: Float = .pi
}
func testKeyPathIterable(x: TestKeyPathIterable) {
  _ = x.allDifferentiableVariables.allKeyPaths
}

// Test type with user-defined memberwise initializer.
struct TF_25: Differentiable {
  public let bar: Float
  public init(bar: Float) {
    self.bar = bar
  }
}
// Test user-defined memberwise initializer.
struct TF_25_Generic<T : Differentiable>: Differentiable {
  public let bar: T
  public init(bar: T) {
    self.bar = bar
  }
}

// Test initializer that is not a memberwise initializer because of stored property name vs parameter label mismatch.
struct HasCustomNonMemberwiseInitializer<T : Differentiable>: Differentiable {
  var value: T
  init(randomLabel value: T) { self.value = value }
}

// Test type with generic environment.
struct HasGenericEnvironment<Scalar : FloatingPoint & Differentiable> : Differentiable {
  var x: Float
}

// Test type with generic members that conform to `Differentiable`.
struct GenericSynthesizeAllStructs<T : Differentiable> : Differentiable {
  var w: T
  var b: T
}

// Test type in generic context.
struct A<T : Differentiable> {
  struct B<U : Differentiable, V> : Differentiable {
    struct InGenericContext : Differentiable {
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
extension Extended : Differentiable {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : Differentiable where T : Differentiable {}

// Test constrained extension of generic type.
struct GenericConstrained<T> {
  var x: T
}
extension GenericConstrained : Differentiable
  where T : Differentiable {}

struct TF_260<T : Differentiable> : Differentiable & AdditiveArithmetic {
  var x: T.TangentVector
}

// TF-269: Test crash when differentiation properties have no getter.
// Related to access levels and associated type inference.
public protocol TF_269_Layer: Differentiable & KeyPathIterable
  where AllDifferentiableVariables: KeyPathIterable {

  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  func applied(to input: Input) -> Output
}

public struct TF_269 : TF_269_Layer {
  public var filter: Float
  public typealias Activation = @differentiable (Output) -> Output
  @noDerivative public let activation: Activation

  public func applied(to input: Float) -> Float {
    return input
  }
}

// Test errors.

// Test manually customizing vector space types.
// Thees should fail. Synthesis is semantically unsupported if vector space
// types are customized.
// expected-error @+1 {{type 'VectorSpaceTypeAlias' does not conform to protocol 'Differentiable'}}
struct VectorSpaceTypeAlias : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = Simple
}
// expected-error @+1 {{type 'VectorSpaceCustomStruct' does not conform to protocol 'Differentiable'}}
struct VectorSpaceCustomStruct : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  struct TangentVector : AdditiveArithmetic, Differentiable {
    var w: Float.TangentVector
    var b: Float.TangentVector
    typealias TangentVector = VectorSpaceCustomStruct.TangentVector
  }
}

struct StaticNoDerivative : Differentiable {
  @noDerivative static var s: Bool = true // expected-error {{'@noDerivative' is only allowed on stored properties in structure types that declare a conformance to 'Differentiable'}}
}

struct StaticMembersShouldNotAffectAnything : AdditiveArithmetic, Differentiable {
  static var x: Bool = true
  static var y: Bool = false
}

struct ImplicitNoDerivative : Differentiable {
  var a: Float
  var b: Bool // expected-warning {{stored property 'b' has no derivative because it does not conform to 'Differentiable'; add '@noDerivative' to make it explicit}}
}

struct ImplicitNoDerivativeWithSeparateTangent : Differentiable {
  var x: DifferentiableSubset
  var b: Bool // expected-warning {{stored property 'b' has no derivative because it does not conform to 'Differentiable'; add '@noDerivative' to make it explicit}} {{3-3=@noDerivative }}
}

// TF-265: Test invalid initializer (that uses a non-existent type).
struct InvalidInitializer : Differentiable {
  init(filterShape: (Int, Int, Int, Int), blah: NonExistentType) {} // expected-error {{use of undeclared type 'NonExistentType'}}
}

// Test derived conformances in disallowed contexts.

// expected-error @+2 {{type 'OtherFileNonconforming' does not conform to protocol 'Differentiable'}}
// expected-error @+1 {{implementation of 'Differentiable' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : Differentiable {}

// expected-error @+2 {{type 'GenericOtherFileNonconforming<T>' does not conform to protocol 'Differentiable'}}
// expected-error @+1 {{implementation of 'Differentiable' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : Differentiable {}
