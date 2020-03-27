// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -print-ast %s | %FileCheck %s --check-prefix=CHECK-AST

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
