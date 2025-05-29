// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/class_differentiable_other_module.swift

import _Differentiation

// Verify that a type `T` conforms to `AdditiveArithmetic`.
func assertConformsToAdditiveArithmetic<T>(_: T.Type)
where T: AdditiveArithmetic {}

// Dummy protocol with default implementations for `AdditiveArithmetic` requirements.
// Used to test `Self : AdditiveArithmetic` requirements.
protocol DummyAdditiveArithmetic: AdditiveArithmetic {}
extension DummyAdditiveArithmetic {
  static func == (lhs: Self, rhs: Self) -> Bool {
    fatalError()
  }
  static var zero: Self {
    fatalError()
  }
  static func + (lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  static func - (lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
}

class Empty: Differentiable {}
func testEmpty() {
  assertConformsToAdditiveArithmetic(Empty.TangentVector.self)
}

protocol DifferentiableWithNonmutatingMoveAlong: Differentiable {}
extension DifferentiableWithNonmutatingMoveAlong {
  func move(by _: TangentVector) {}
}

class EmptyWithInheritedNonmutatingMoveAlong: DifferentiableWithNonmutatingMoveAlong {
  typealias TangentVector = Empty.TangentVector
  static func proof_that_i_have_nonmutating_move_along() {
    let empty = EmptyWithInheritedNonmutatingMoveAlong()
    empty.move(by: .init())
  }
}

class EmptyWrapper<T: Differentiable & AnyObject>: Differentiable {}
func testEmptyWrapper() {
  assertConformsToAdditiveArithmetic(Empty.TangentVector.self)
  assertConformsToAdditiveArithmetic(EmptyWrapper<Empty>.TangentVector.self)
}

// Test structs with `let` stored properties.
// Derived conformances fail because `mutating func move` requires all stored
// properties to be mutable.
class ImmutableStoredProperties<T: Differentiable & AnyObject>: Differentiable {
  var okay: Float

  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Int' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let nondiff: Int

  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'ImmutableStoredProperties' requires all stored properties not marked with '@noDerivative' to be mutable or have a non-mutating 'move(by:)'; use 'var' instead, or add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let diff: Float

  let letClass: Empty // No error on class-bound differentiable `let` with a non-mutating 'move(by:)'.

  let letClassWithInheritedNonmutatingMoveAlong: EmptyWithInheritedNonmutatingMoveAlong

  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'ImmutableStoredProperties' requires all stored properties not marked with '@noDerivative' to be mutable or have a non-mutating 'move(by:)'; use 'var' instead, or add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let letClassGeneric: T // Error due to lack of non-mutating 'move(by:)'.

  let letClassWrappingGeneric: EmptyWrapper<T> // No error on class-bound differentiable `let` with a non-mutating 'move(by:)'.

  init(letClassGeneric: T) {
    okay = 0
    nondiff = 0
    diff = 0
    letClass = Empty()
    self.letClassGeneric = letClassGeneric
    self.letClassWrappingGeneric = EmptyWrapper<T>()
  }
}
func testImmutableStoredProperties() {
  _ = ImmutableStoredProperties<Empty>.TangentVector(
    okay: 1, 
    letClass: Empty.TangentVector(), 
    letClassWithInheritedNonmutatingMoveAlong: Empty.TangentVector(), 
    letClassWrappingGeneric: EmptyWrapper<Empty>.TangentVector())
}
class MutableStoredPropertiesWithInitialValue: Differentiable {
  var x = Float(1)
  var y = Double(1)
}
// Test class with both an empty constructor and memberwise initializer.
class AllMixedStoredPropertiesHaveInitialValue: Differentiable {
  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'AllMixedStoredPropertiesHaveInitialValue' requires all stored properties not marked with '@noDerivative' to be mutable or have a non-mutating 'move(by:)'; use 'var' instead, or add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}
  let x = Float(1)
  var y = Float(1)
  // Memberwise initializer should be `init(y:)` since `x` is immutable.
  static func testMemberwiseInitializer() {
    _ = AllMixedStoredPropertiesHaveInitialValue()
  }
}
/*
class HasCustomConstructor: Differentiable {
  var x = Float(1)
  var y = Float(1)
  // Custom constructor should not affect synthesis.
  init(x: Float, y: Float, z: Bool) {}
}
*/

class Simple: Differentiable {
  var w: Float
  var b: Float

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}
func testSimple() {
  let simple = Simple(w: 1, b: 1)
  let tangent = Simple.TangentVector(w: 1, b: 1)
  simple.move(by: tangent)
}

// Test type with mixed members.
class Mixed: Differentiable {
  var simple: Simple
  var float: Float

  init(simple: Simple, float: Float) {
    self.simple = simple
    self.float = float
  }
}
func testMixed(_ simple: Simple, _ simpleTangent: Simple.TangentVector) {
  let mixed = Mixed(simple: simple, float: 1)
  let tangent = Mixed.TangentVector(simple: simpleTangent, float: 1)
  mixed.move(by: tangent)
}

// Test type with manual definition of vector space types to `Self`.
final class VectorSpacesEqualSelf: Differentiable & DummyAdditiveArithmetic {
  var w: Float
  var b: Float
  typealias TangentVector = VectorSpacesEqualSelf

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}
/*
extension VectorSpacesEqualSelf : Equatable, AdditiveArithmetic {
  static func == (lhs: VectorSpacesEqualSelf, rhs: VectorSpacesEqualSelf) -> Bool {
    fatalError()
  }
  static var zero: VectorSpacesEqualSelf {
    fatalError()
  }
  static func + (lhs: VectorSpacesEqualSelf, rhs: VectorSpacesEqualSelf) -> VectorSpacesEqualSelf {
    fatalError()
  }
  static func - (lhs: VectorSpacesEqualSelf, rhs: VectorSpacesEqualSelf) -> VectorSpacesEqualSelf {
    fatalError()
  }
}
*/

// Test generic type with vector space types to `Self`.
class GenericVectorSpacesEqualSelf<T>: Differentiable
where T: Differentiable, T == T.TangentVector {
  var w: T
  var b: T

  init(w: T, b: T) {
    self.w = w
    self.b = b
  }
}
func testGenericVectorSpacesEqualSelf() {
  let genericSame = GenericVectorSpacesEqualSelf<Double>(w: 1, b: 1)
  let tangent = GenericVectorSpacesEqualSelf.TangentVector(w: 1, b: 1)
  genericSame.move(by: tangent)
}

// Test nested type.
class Nested: Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>

  init(
    simple: Simple, mixed: Mixed, generic: GenericVectorSpacesEqualSelf<Double>
  ) {
    self.simple = simple
    self.mixed = mixed
    self.generic = generic
  }
}
func testNested(
  _ simple: Simple, _ mixed: Mixed,
  _ genericSame: GenericVectorSpacesEqualSelf<Double>
) {
  _ = Nested(simple: simple, mixed: mixed, generic: genericSame)
}

// Test type that does not conform to `AdditiveArithmetic` but whose members do.
// Thus, `Self` cannot be used as `TangentVector` or `TangentVector`.
// Vector space structs types must be synthesized.
// Note: it would be nice to emit a warning if conforming `Self` to
// `AdditiveArithmetic` is possible.
class AllMembersAdditiveArithmetic: Differentiable {
  var w: Float
  var b: Float

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}

// Test type whose properties are not all differentiable.
class DifferentiableSubset: Differentiable {
  var w: Float
  var b: Float
  @noDerivative var flag: Bool
  @noDerivative let technicallyDifferentiable: Float = .pi

  init(w: Float, b: Float, flag: Bool) {
    self.w = w
    self.b = b
    self.flag = flag
  }
}
func testDifferentiableSubset() {
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
  _ = DifferentiableSubset.TangentVector(w: 1, b: 1)
}

// Test nested type whose properties are not all differentiable.
class NestedDifferentiableSubset: Differentiable {
  var x: DifferentiableSubset
  var mixed: Mixed
  @noDerivative var technicallyDifferentiable: Float

  init(x: DifferentiableSubset, mixed: Mixed) {
    self.x = x
    self.mixed = mixed
    technicallyDifferentiable = 0
  }
}

// Test type that uses synthesized vector space types but provides custom
// method.
class HasCustomMethod: Differentiable {
  var simple: Simple
  var mixed: Mixed
  var generic: GenericVectorSpacesEqualSelf<Double>

  init(
    simple: Simple, mixed: Mixed, generic: GenericVectorSpacesEqualSelf<Double>
  ) {
    self.simple = simple
    self.mixed = mixed
    self.generic = generic
  }

  func move(by offset: TangentVector) {
    print("Hello world")
    simple.move(by: offset.simple)
    mixed.move(by: offset.mixed)
    generic.move(by: offset.generic)
  }
}

// Test type with user-defined memberwise initializer.
class TF_25: Differentiable {
  public var bar: Float
  public init(bar: Float) {
    self.bar = bar
  }
}
// Test user-defined memberwise initializer.
class TF_25_Generic<T: Differentiable>: Differentiable {
  public var bar: T
  public init(bar: T) {
    self.bar = bar
  }
}

// Test initializer that is not a memberwise initializer because of stored property name vs parameter label mismatch.
class HasCustomNonMemberwiseInitializer<T: Differentiable>: Differentiable {
  var value: T
  init(randomLabel value: T) { self.value = value }
}

// Test type with generic environment.
class HasGenericEnvironment<Scalar: FloatingPoint & Differentiable>: Differentiable
{
  var x: Float = 0
}

// Test type with generic members that conform to `Differentiable`.
class GenericSynthesizeAllStructs<T: Differentiable>: Differentiable {
  var w: T
  var b: T

  init(w: T, b: T) {
    self.w = w
    self.b = b
  }
}

// Test type in generic context.
class A<T: Differentiable> {
  class B<U: Differentiable, V>: Differentiable {
    class InGenericContext: Differentiable {
      @noDerivative var a: A
      var b: B
      var t: T
      var u: U

      init(a: A, b: B, t: T, u: U) {
        self.a = a
        self.b = b
        self.t = t
        self.u = u
      }
    }
  }
}

// Test extension.
class Extended {
  var x: Float

  init(x: Float) {
    self.x = x
  }
}
extension Extended: Differentiable {}

// Test extension of generic type.
class GenericExtended<T> {
  var x: T

  init(x: T) {
    self.x = x
  }
}
extension GenericExtended: Differentiable where T: Differentiable {}

// Test constrained extension of generic type.
class GenericConstrained<T> {
  var x: T

  init(x: T) {
    self.x = x
  }
}
extension GenericConstrained: Differentiable
where T: Differentiable {}

final class TF_260<T: Differentiable>: Differentiable & DummyAdditiveArithmetic
{
  var x: T.TangentVector

  init(x: T.TangentVector) {
    self.x = x
  }
}

// TF-269: Test crash when differentiation properties have no getter.
// Related to access levels and associated type inference.

// TODO(TF-631): Blocked by class type differentiation support.
// [AD] Unhandled instruction in adjoint emitter:   %2 = ref_element_addr %0 : $TF_269, #TF_269.filter // user: %3
// [AD] Diagnosing non-differentiability.
// [AD] For instruction:
//   %2 = ref_element_addr %0 : $TF_269, #TF_269.filter // user: %3
/*
public protocol TF_269_Layer: Differentiable & KeyPathIterable
  where TangentVector: KeyPathIterable {

  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  func applied(to input: Input) -> Output
}

public class TF_269 : TF_269_Layer {
  public var filter: Float
  public typealias Activation = @differentiable(reverse) (Output) -> Output
  @noDerivative public let activation: @differentiable(reverse) (Output) -> Output

  init(filter: Float, activation: @escaping Activation) {
    self.filter = filter
    self.activation = activation
  }

  // NOTE: `KeyPathIterable` derived conformances do not yet support class
  // types.
  public var allKeyPaths: [PartialKeyPath<TF_269>] {
    []
  }

  public func applied(to input: Float) -> Float {
    return input
  }
}
*/

// Test errors.

// expected-error @+1 {{class 'MissingInitializer' has no initializers}}
class MissingInitializer: Differentiable {
  // expected-note @+1 {{stored property 'w' without initial value prevents synthesized initializers}}
  var w: Float
  // expected-note @+1 {{stored property 'b' without initial value prevents synthesized initializers}}
  var b: Float
}

// Test manually customizing vector space types.
// These should fail. Synthesis is semantically unsupported if vector space
// types are customized.
final class TangentVectorWB: DummyAdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}
// expected-error @+1 {{type 'VectorSpaceTypeAlias' does not conform to protocol 'Differentiable'}}
final class VectorSpaceTypeAlias: DummyAdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  typealias TangentVector = TangentVectorWB

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}
// expected-error @+1 {{type 'VectorSpaceCustomStruct' does not conform to protocol 'Differentiable'}}
final class VectorSpaceCustomStruct: DummyAdditiveArithmetic, Differentiable {
  var w: Float
  var b: Float
  struct TangentVector: AdditiveArithmetic, Differentiable {
    var w: Float.TangentVector
    var b: Float.TangentVector
    typealias TangentVector = VectorSpaceCustomStruct.TangentVector
  }

  init(w: Float, b: Float) {
    self.w = w
    self.b = b
  }
}

class StaticNoDerivative: Differentiable {
  @noDerivative static var s: Bool = true
}

final class StaticMembersShouldNotAffectAnything: DummyAdditiveArithmetic,
  Differentiable
{
  static var x: Bool = true
  static var y: Bool = false
}

class ImplicitNoDerivative: Differentiable {
  var a: Float = 0
  var b: Bool = true  // expected-warning {{stored property 'b' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
}

class ImplicitNoDerivativeWithSeparateTangent: Differentiable {
  var x: DifferentiableSubset
  var b: Bool = true  // expected-warning {{stored property 'b' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}} {{3-3=@noDerivative }}

  init(x: DifferentiableSubset) {
    self.x = x
  }
}

// TF-1018: verify that `@noDerivative` warnings are always silenceable, even
// when the `Differentiable` conformance context is not the nominal type
// declaration.

class ExtensionDifferentiableNoDerivative<T> {
  // expected-warning @+2 {{stored property 'x' has no derivative because 'T' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  // expected-warning @+1 {{stored property 'y' has no derivative because 'T' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var x, y: T
  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var nondiff: Bool
  init() { fatalError() }
}
extension ExtensionDifferentiableNoDerivative: Differentiable {}

class ExtensionDifferentiableNoDerivativeFixed<T> {
  @noDerivative var x, y: T
  @noDerivative var nondiff: Bool
  init() { fatalError() }
}
extension ExtensionDifferentiableNoDerivativeFixed: Differentiable {}

class ConditionalDifferentiableNoDerivative<T> {
  var x, y: T
  // expected-warning @+1 {{stored property 'nondiff' has no derivative because 'Bool' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  var nondiff: Bool
  init() { fatalError() }
}
extension ConditionalDifferentiableNoDerivative: Differentiable
where T: Differentiable {}

class ConditionalDifferentiableNoDerivativeFixed<T> {
  var x, y: T
  @noDerivative var nondiff: Bool
  init() { fatalError() }
}
extension ConditionalDifferentiableNoDerivativeFixed: Differentiable
where T: Differentiable {}

// TF-265: Test invalid initializer (that uses a non-existent type).
class InvalidInitializer: Differentiable {
  init(filterShape: (Int, Int, Int, Int), blah: NonExistentType) {}  // expected-error {{cannot find type 'NonExistentType' in scope}}
}

// Test memberwise initializer synthesis.
final class NoMemberwiseInitializerExtended<T> {
  var value: T
  init(_ value: T) {
    self.value = value
  }
}
extension NoMemberwiseInitializerExtended: Equatable, AdditiveArithmetic,
  DummyAdditiveArithmetic
where T: AdditiveArithmetic {}
extension NoMemberwiseInitializerExtended: Differentiable
where T: Differentiable & AdditiveArithmetic {}

// https://github.com/apple/swift/issues/55238
// Test interaction with `@differentiable` and `@derivative` type-checking.

class C_55238: Differentiable {
  @differentiable(reverse)
  var x: Float = 0

  @differentiable(reverse)
  func method() -> Float { x }

  @derivative(of: method)
  func vjpMethod() -> (value: Float, pullback: (Float) -> TangentVector) { fatalError() }

  // Test usage of synthesized `TangentVector` type.
  // This should not produce an error: "reference to invalid associated type 'TangentVector'".
  func move(by offset: TangentVector) {}
}

// Test property wrappers.

@propertyWrapper
struct ImmutableWrapper<Value> {
  private var value: Value
  var wrappedValue: Value { value }
  init(wrappedValue: Value) {
    self.value = wrappedValue
  }
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

class WrappedProperties: Differentiable {
  // expected-warning @+1 {{synthesis of the 'Differentiable.move(by:)' requirement for 'WrappedProperties' requires 'wrappedValue' in property wrapper 'ImmutableWrapper' to be mutable or have a non-mutating 'move(by:)'; add an explicit '@noDerivative' attribute}}
  @ImmutableWrapper var immutableInt: Generic<Int> = Generic()

  // expected-warning @+1 {{stored property 'mutableInt' has no derivative because 'Generic<Int>' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}
  @Wrapper var mutableInt: Generic<Int> = Generic()

  @Wrapper var float: Generic<Float> = Generic()
  @ClassWrapper var float2: Generic<Float> = Generic()

  // https://github.com/apple/swift/issues/55517
  // Test `@differentiable` wrapped property.
  @differentiable(reverse) @Wrapper var float3: Generic<Float> = Generic()

  @noDerivative @ImmutableWrapper var nondiff: Generic<Int> = Generic()

  static func testTangentMemberwiseInitializer() {
    _ = TangentVector(float: .init(), float2: .init(), float3: .init())
  }
}

// Test derived conformances in disallowed contexts.

extension OtherFileNonconforming: Differentiable {}
// expected-error@-1 {{extension outside of file declaring class 'OtherFileNonconforming' prevents automatic synthesis of 'move(by:)' for protocol 'Differentiable'}}
// expected-note@-2 {{add stubs for conformance}}

extension GenericOtherFileNonconforming: Differentiable {}
// expected-error@-1 {{extension outside of file declaring generic class 'GenericOtherFileNonconforming' prevents automatic synthesis of 'move(by:)' for protocol 'Differentiable'}}
// expected-note@-2 {{add stubs for conformance}}
