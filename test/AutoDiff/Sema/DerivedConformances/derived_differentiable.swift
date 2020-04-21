// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefix=CHECK-AST

import _Differentiation

struct GenericTangentVectorMember<T: Differentiable>: Differentiable,
  AdditiveArithmetic
{
  var x: T.TangentVector
}

// CHECK-AST-LABEL: internal struct GenericTangentVectorMember<T> : Differentiable, AdditiveArithmetic where T : Differentiable
// CHECK-AST:   internal var x: T.TangentVector
// CHECK-AST:   internal init(x: T.TangentVector)
// CHECK-AST:   internal typealias TangentVector = GenericTangentVectorMember<T>
// CHECK-AST:   internal static var zero: GenericTangentVectorMember<T> { get }
// CHECK-AST:   internal static func + (lhs: GenericTangentVectorMember<T>, rhs: GenericTangentVectorMember<T>) -> GenericTangentVectorMember<T>
// CHECK-AST:   internal static func - (lhs: GenericTangentVectorMember<T>, rhs: GenericTangentVectorMember<T>) -> GenericTangentVectorMember<T>
// CHECK-AST:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: GenericTangentVectorMember<T>, _ b: GenericTangentVectorMember<T>) -> Bool

public struct ConditionallyDifferentiable<T> {
  public var x: T
}
extension ConditionallyDifferentiable: Differentiable where T: Differentiable {}

// CHECK-AST-LABEL: public struct ConditionallyDifferentiable<T> {
// CHECK-AST:         @differentiable(wrt: self where T : Differentiable)
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
// CHECK-AST:         internal struct TangentVector : Differentiable, AdditiveArithmetic
// CHECK-AST:       }

@frozen
public struct FrozenStruct: Differentiable {}

// CHECK-AST-LABEL: @frozen public struct FrozenStruct : Differentiable {
// CHECK-AST:   internal init()
// CHECK-AST:   @frozen public struct TangentVector : Differentiable, AdditiveArithmetic {

@usableFromInline
struct UsableFromInlineStruct: Differentiable {}

// CHECK-AST-LABEL: @usableFromInline
// CHECK-AST: struct UsableFromInlineStruct : Differentiable {
// CHECK-AST:   internal init()
// CHECK-AST:   @usableFromInline
// CHECK-AST:   struct TangentVector : Differentiable, AdditiveArithmetic {

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
// CHECK-AST:   internal struct TangentVector : Differentiable, AdditiveArithmetic {
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
// CHECK-AST:   internal struct TangentVector : Differentiable, AdditiveArithmetic {
// CHECK-AST:     internal var x: Float.TangentVector
// CHECK-AST:     internal var y: Float.TangentVector
// CHECK-AST:     internal var z: Float.TangentVector
// CHECK-AST:   }
// CHECK-AST: }
