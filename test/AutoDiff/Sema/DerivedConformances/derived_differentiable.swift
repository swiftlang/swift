// RUN: %target-swift-frontend -print-ast-decl %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SIL

import _Differentiation

struct GenericTangentVectorMember<T: Differentiable>: Differentiable,
  AdditiveArithmetic
{
  var x: T.TangentVector
}

// CHECK-AST-LABEL: internal struct GenericTangentVectorMember<T> : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}} where T : Differentiable
// CHECK-AST:   internal var x: T.TangentVector
// CHECK-AST:   internal static func + (lhs: GenericTangentVectorMember<T>, rhs: GenericTangentVectorMember<T>) -> GenericTangentVectorMember<T>
// CHECK-AST:   internal static func - (lhs: GenericTangentVectorMember<T>, rhs: GenericTangentVectorMember<T>) -> GenericTangentVectorMember<T>
// CHECK-AST:   internal typealias TangentVector = GenericTangentVectorMember<T>
// CHECK-AST:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: GenericTangentVectorMember<T>, _ b: GenericTangentVectorMember<T>) -> Bool
// CHECK-AST:   internal init(x: T.TangentVector)
// CHECK-AST:   internal static var zero: GenericTangentVectorMember<T> { get }

public struct ConditionallyDifferentiable<T> {
  public var x: T
}
extension ConditionallyDifferentiable: Differentiable where T: Differentiable {}

// CHECK-AST-LABEL: public struct ConditionallyDifferentiable<T> {
// CHECK-AST:         @differentiable(reverse, wrt: self where T : Differentiable)
// CHECK-AST:         public var x: T
// CHECK-AST:         internal init(x: T)
// CHECK-AST:       }

// Verify that `TangentVector` is not synthesized to be `Self` for
// `AdditiveArithmetic`-conforming classes.
final class AdditiveArithmeticClass<T: AdditiveArithmetic & Differentiable>: AdditiveArithmetic,
  Differentiable
{
  var x, y: T
  init(x: T, y: T) {
    self.x = x
    self.y = y
  }

  // Dummy `AdditiveArithmetic` requirements.
  static func == (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass)
    -> Bool
  {
    fatalError()
  }
  static var zero: AdditiveArithmeticClass {
    fatalError()
  }
  static func + (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass)
    -> Self
  {
    fatalError()
  }
  static func - (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass)
    -> Self
  {
    fatalError()
  }
}

// CHECK-AST-LABEL: final internal class AdditiveArithmeticClass<T> : AdditiveArithmetic, Differentiable where T : AdditiveArithmetic, T : Differentiable {
// CHECK-AST:         final internal var x: T, y: T
// CHECK-AST:         internal struct TangentVector : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}}
// CHECK-AST:       }

@frozen
public struct FrozenStruct: Differentiable {}

// CHECK-AST-LABEL: @frozen public struct FrozenStruct : Differentiable {
// CHECK-AST:   @frozen public struct TangentVector : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}} {
// CHECK-AST:   internal init()

@usableFromInline
struct UsableFromInlineStruct: Differentiable {}

// CHECK-AST-LABEL: @usableFromInline
// CHECK-AST: struct UsableFromInlineStruct : Differentiable {
// CHECK-AST:   @usableFromInline
// CHECK-AST:   struct TangentVector : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}} {
// CHECK-AST:   internal init()

// Test property wrappers.

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct WrappedPropertiesStruct: Differentiable {
  @Wrapper @Wrapper var x: Float
  @Wrapper var y: Float
  var z: Float
  @noDerivative @Wrapper var nondiff: Float
}

// CHECK-AST-LABEL: internal struct WrappedPropertiesStruct : Differentiable {
// CHECK-AST:   internal struct TangentVector : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}} {
// CHECK-AST:     internal var x: Float.TangentVector
// CHECK-AST:     internal var y: Float.TangentVector
// CHECK-AST:     internal var z: Float.TangentVector
// CHECK-AST:   }
// CHECK-AST: }

class WrappedPropertiesClass: Differentiable {
  @Wrapper @Wrapper var x: Float = 1
  @Wrapper var y: Float = 2
  var z: Float = 3
  @noDerivative @Wrapper var noDeriv: Float = 4
}

// CHECK-AST-LABEL: internal class WrappedPropertiesClass : Differentiable {
// CHECK-AST:   internal struct TangentVector : {{(Differentiable, AdditiveArithmetic)|(AdditiveArithmetic, Differentiable)}} {
// CHECK-AST:     internal var x: Float.TangentVector
// CHECK-AST:     internal var y: Float.TangentVector
// CHECK-AST:     internal var z: Float.TangentVector
// CHECK-AST:   }
// CHECK-AST: }

protocol TangentVectorMustBeEncodable: Differentiable where TangentVector: Encodable {}

struct AutoDeriveEncodableTV1: TangentVectorMustBeEncodable {
  var x: Float
}

// CHECK-AST-LABEL: internal struct AutoDeriveEncodableTV1 : TangentVectorMustBeEncodable {
// CHECK-AST:   internal struct TangentVector : {{(Encodable, Differentiable, AdditiveArithmetic)|(Encodable, AdditiveArithmetic, Differentiable)|(Differentiable, Encodable, AdditiveArithmetic)|(AdditiveArithmetic, Encodable, Differentiable)|(Differentiable, AdditiveArithmetic, Encodable)|(AdditiveArithmetic, Differentiable, Encodable)}} {

struct AutoDeriveEncodableTV2 {
  var x: Float
}

extension AutoDeriveEncodableTV2: TangentVectorMustBeEncodable {}

// CHECK-AST-LABEL: extension AutoDeriveEncodableTV2 : TangentVectorMustBeEncodable {
// CHECK-AST:   internal struct TangentVector : {{(Encodable, Differentiable, AdditiveArithmetic)|(Encodable, AdditiveArithmetic, Differentiable)|(Differentiable, Encodable, AdditiveArithmetic)|(AdditiveArithmetic, Encodable, Differentiable)|(Differentiable, AdditiveArithmetic, Encodable)|(AdditiveArithmetic, Differentiable, Encodable)}} {

protocol TangentVectorP: Differentiable {
  var requirement: Int { get }
}

protocol TangentVectorConstrained: Differentiable where TangentVector: TangentVectorP {}

struct StructWithTangentVectorConstrained: TangentVectorConstrained {
  var x: Float
}

// `extension StructWithTangentVectorConstrained.TangentVector: TangentVectorP` gives
// "error: type 'StructWithTangentVectorConstrained.TangentVector' does not conform to protocol 'TangentVectorP'",
// maybe because it typechecks the conformance before seeing the extension. But this roundabout way
// of stating the same thing works.
extension TangentVectorP where Self == StructWithTangentVectorConstrained.TangentVector {
  var requirement: Int { 42 }
}

// CHECK-AST-LABEL: internal struct StructWithTangentVectorConstrained : TangentVectorConstrained {
// CHECK-AST:   internal struct TangentVector : {{(TangentVectorP, Differentiable, AdditiveArithmetic)|(TangentVectorP, AdditiveArithmetic, Differentiable)|(Differentiable, TangentVectorP, AdditiveArithmetic)|(AdditiveArithmetic, TangentVectorP, Differentiable)|(Differentiable, AdditiveArithmetic, TangentVectorP)|(AdditiveArithmetic, Differentiable, TangentVectorP)}} {

// https://github.com/apple/swift/issues/56601

public struct S1: Differentiable {
  public var simd: [Float]
  public var scalar: Float
}

// CHECK-AST-LABEL: public struct S1 : Differentiable {
// CHECK-AST: public var simd: [Float]
// CHECK-AST: public var scalar: Float
// CHECK-AST: struct TangentVector : AdditiveArithmetic, Differentiable {
// CHECK-AST:   var simd: Array<Float>.TangentVector
// CHECK-AST:   var scalar: Float

// CHECK-SIL-LABEL: public struct S1 : Differentiable {
// CHECK-SIL: @differentiable(reverse, wrt: self)
// CHECK-SIL: @_hasStorage public var simd: [Float] { get set }
// CHECK-SIL: @differentiable(reverse, wrt: self)
// CHECK-SIL: @_hasStorage public var scalar: Float { get set }
// CHECK-SIL: struct TangentVector : AdditiveArithmetic, Differentiable {
// CHECK-SIL:   @_hasStorage var simd: Array<Float>.DifferentiableView { get set }
// CHECK-SIL:   @_hasStorage var scalar: Float { get set }
