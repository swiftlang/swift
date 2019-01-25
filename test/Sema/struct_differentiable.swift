// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s

// Verify that a `Differentiable` type upholds `AllDifferentiableVariables == CotangentVector`.
func assertAllDifferentiableVariablesEqualsCotangentVector<T>(_: T.Type)
  where T : Differentiable, T.AllDifferentiableVariables == T.CotangentVector {}

// Verify that a type `T` conforms to `AdditiveArithmetic`.
func assertConformsToAdditiveArithmetic<T>(_: T.Type) where T : AdditiveArithmetic {}

// Verify that a type `T` conforms to `VectorNumeric`.
func assertConformsToVectorNumeric<T>(_: T.Type) where T : VectorNumeric {}

struct Empty : Differentiable {}
assertConformsToAdditiveArithmetic(Empty.AllDifferentiableVariables.self)

// Test interaction with `AdditiveArithmetic` derived conformances.
// Previously, this crashed due to duplicate memberwise initializer synthesis.
struct EmptyAdditiveArithmetic : AdditiveArithmetic, Differentiable {}

// Test structs whose stored properties all have an initial value.
struct AllLetStoredPropertiesHaveInitialValue : Differentiable { // expected-error {{does not conform to protocol 'Differentiable'}}
  let x = Float(1)
  let y = Float(1)
}
struct AllVarStoredPropertiesHaveInitialValue : Differentiable {
  var x = Float(1)
  var y = Float(1)
}
// Test struct with both an empty constructor and memberwise initializer.
struct AllMixedStoredPropertiesHaveInitialValue : Differentiable { // expected-error {{does not conform to protocol 'Differentiable'}}
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

struct Simple : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
}
var simple = Simple(w: 1, b: 1)
simple.allDifferentiableVariables = simple + simple
assert(simple.moved(along: simple) == simple + simple)
assert(simple.tangentVector(from: simple) == simple)

// Test type with mixed members.
struct Mixed : AdditiveArithmetic, Differentiable {
  var simple: Simple
  var float: Float
}
var mixed = Mixed(simple: simple, float: 1)
mixed.allDifferentiableVariables = Mixed(simple: simple, float: 2)
assert(mixed.moved(along: mixed) == mixed + mixed)
assert(mixed.tangentVector(from: mixed) == mixed)

// Test type with manual definition of vector space types to `Self`.
struct VectorSpacesEqualSelf : AdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = VectorSpacesEqualSelf
  typealias CotangentVector = VectorSpacesEqualSelf
  typealias AllDifferentiableVariables = VectorSpacesEqualSelf
}

// Test generic type with vector space types to `Self`.
struct GenericVectorSpacesEqualSelf<T> : AdditiveArithmetic, Differentiable
  where T : AdditiveArithmetic, T : Differentiable,
        T == T.TangentVector, T == T.CotangentVector
{
  var w: T
  var b: T
}
var genericSame = GenericVectorSpacesEqualSelf<Double>(w: 1, b: 1)
genericSame.allDifferentiableVariables = genericSame + genericSame
assert(genericSame.moved(along: genericSame) == genericSame + genericSame)
assert(genericSame.tangentVector(from: genericSame) == genericSame)

// Test nested type.
struct Nested : AdditiveArithmetic, Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>
}
let nested = Nested(simple: simple, mixed: mixed, generic: genericSame)
assert(nested.moved(along: nested) == nested + nested)
assert(nested.tangentVector(from: nested) == nested)

_ = pullback(at: Nested(simple: simple, mixed: mixed, generic: genericSame)) { model in
  model.simple + model.simple
}

// Test type that does not conform to `AdditiveArithmetic` but whose members do.
// Thus, `Self` cannot be used as `TangentVector` or `CotangentVector`.
// Vector space structs types must be synthesized.
// Note: it would be nice to emit a warning if conforming `Self` to
// `AdditiveArithmetic` is possible.
struct AllMembersAdditiveArithmetic : Differentiable {
  var w: Float
  var b: Float
}
assertAllDifferentiableVariablesEqualsCotangentVector(AllMembersAdditiveArithmetic.self)

// Test type `AllMembersVectorNumeric` whose members conforms to `VectorNumeric`,
// in which case we should make `TangentVector` and `CotangentVector` conform to
// `VectorNumeric`.
struct MyVector : VectorNumeric, Differentiable {
  var w: Float
  var b: Float
}
struct AllMembersVectorNumeric : Differentiable {
  var w: MyVector
  var b: MyVector
}
assertConformsToVectorNumeric(AllMembersVectorNumeric.TangentVector.self)
assertConformsToVectorNumeric(AllMembersVectorNumeric.CotangentVector.self)

// Test type with immutable, differentiable stored property.
struct ImmutableStoredProperty : Differentiable { // expected-error {{does not conform to protocol 'Differentiable'}}
  var w: Float
  let fixedBias: Float = .pi
}
_ = ImmutableStoredProperty.TangentVector(w: 1, fixedBias: 1)

// Test type whose properties are not all differentiable.
struct DifferentiableSubset : Differentiable {
  var w: Float
  var b: Float
  @noDerivative var flag: Bool
  @noDerivative let technicallyDifferentiable: Float = .pi
}
assertAllDifferentiableVariablesEqualsCotangentVector(DifferentiableSubset.self)
let tangentSubset = DifferentiableSubset.TangentVector(w: 1, b: 1)
let cotangentSubset = DifferentiableSubset.CotangentVector(w: 1, b: 1)
let allDiffVarsSubset = DifferentiableSubset.AllDifferentiableVariables(w: 1, b: 1)

_ = pullback(at: DifferentiableSubset(w: 1, b: 2, flag: false)) { model in
  model.w + model.b
}

// Test nested type whose properties are not all differentiable.
struct NestedDifferentiableSubset : Differentiable {
  var x: DifferentiableSubset
  var mixed: Mixed
  @noDerivative var technicallyDifferentiable: Float
}
assertAllDifferentiableVariablesEqualsCotangentVector(NestedDifferentiableSubset.self)

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

// Test type with generic environment.
struct HasGenericEnvironment<Scalar : FloatingPoint & Differentiable> : Differentiable {
  var x: Float
}

/*
// Test type with generic members that conform to `Differentiable`.
// Since it's not the case that
// `T == T.TangentVector == T.CotangentVector`,
// it's necessary to synthesize new vector space struct types.

// FIXME: Blocked by bug, potentially related to SR-9595.
// Type checker is unable to infer `T.TangentVector : AdditiveArithmetic` due
// to mutually recursive constraints:
// - `TangentVector.CotangentVector == CotangentVector`
// - `CotangentVector.CotangentVector == TangentVector`
struct GenericNeedsVectorSpaceStructs<T> : Differentiable
  where T : Differentiable
{
  var w: T
  var b: T
}

// Test generic type with vector space types to `Self`.
struct GenericNotAdditiveArithmetic<T> : Differentiable
  where T : Differentiable, T == T.TangentVector, T == T.CotangentVector
{
  var w: T
  var b: T
}

// Test type in generic context.
// FIXME: Blocked by bug, potentially related to SR-9595.
struct A<T : Differentiable> {
  struct B<U : Differentiable, V> : Differentiable {
    struct InGenericContext {
      var w: T
      var b: U
    }
  }
}
*/

// Test errors.

// Test manually customizing vector space types.
// Thees should fail. Synthesis is semantically unsupported if vector space
// types are customized.
struct VectorSpaceTypeAlias : AdditiveArithmetic, Differentiable { // expected-error {{type 'VectorSpaceTypeAlias' does not conform to protocol 'Differentiable'}}
  var w: Float
  var b: Float
  typealias TangentVector = Simple
}
struct VectorSpaceCustomStruct : AdditiveArithmetic, Differentiable { // expected-error {{type 'VectorSpaceCustomStruct' does not conform to protocol 'Differentiable'}}
  var w: Float
  var b: Float
  struct CotangentVector : AdditiveArithmetic, Differentiable {
    var w: Float.CotangentVector
    var b: Float.CotangentVector
    typealias TangentVector = VectorSpaceCustomStruct.CotangentVector
    typealias CotangentVector = VectorSpaceCustomStruct.CotangentVector
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
  var b: Bool // expected-warning {{stored property has no derivative because it does not conform to 'Differentiable'; add '@noDerivative' to make it explicit}}
}

struct ImplicitNoDerivativeWithSeparateTangent : Differentiable {
  var x: DifferentiableSubset
  var b: Bool // expected-warning {{stored property has no derivative because it does not conform to 'Differentiable'; add '@noDerivative' to make it explicit}} {{3-3=@noDerivative }}
}
