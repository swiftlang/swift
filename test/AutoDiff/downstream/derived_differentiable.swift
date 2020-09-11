// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s

struct PointwiseMultiplicativeDummy : EuclideanDifferentiable, PointwiseMultiplicative {}

public struct Foo : EuclideanDifferentiable {
  public var a: Float
}

// CHECK-AST-LABEL: public struct Foo : EuclideanDifferentiable {
// CHECK-AST:   @differentiable
// CHECK-AST:   public var a: Float
// CHECK-AST:   internal init(a: Float)
// CHECK-AST:   public struct TangentVector
// CHECK-AST:     public typealias TangentVector = Foo.TangentVector
// CHECK-AST:   public var differentiableVectorView: Foo.TangentVector { get }

// CHECK-SILGEN-LABEL: // differentiability witness for Foo.a.getter
// CHECK-SILGEN-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0]

struct AdditiveTangentIsSelf : AdditiveArithmetic, EuclideanDifferentiable {
  var a: Float
  var dummy: PointwiseMultiplicativeDummy
}
let _: @differentiable (AdditiveTangentIsSelf) -> Float = { x in
  x.a + x.a
}

// CHECK-AST-LABEL: internal struct AdditiveTangentIsSelf : AdditiveArithmetic, EuclideanDifferentiable {
// CHECK-AST:         internal var a: Float
// CHECK-AST:         internal var dummy: PointwiseMultiplicativeDummy
// CHECK-AST:         internal init(a: Float, dummy: PointwiseMultiplicativeDummy)
// CHECK-AST:         internal typealias TangentVector = AdditiveTangentIsSelf
// The following should not exist because when `Self == Self.TangentVector`, `differentiableVectorView` is not synthesized.
// CHECK-AST-NOT:     internal var differentiableVectorView: AdditiveTangentIsSelf { get }

struct TestNoDerivative : EuclideanDifferentiable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: internal struct TestNoDerivative : EuclideanDifferentiable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         internal struct TangentVector : Differentiable, AdditiveArithmetic, ElementaryFunctions
// CHECK-AST:           internal typealias TangentVector = TestNoDerivative.TangentVector
// CHECK-AST:         internal var differentiableVectorView: TestNoDerivative.TangentVector { get }

struct TestPointwiseMultiplicative : Differentiable {
  var w: PointwiseMultiplicativeDummy
  @noDerivative var technicallyDifferentiable: PointwiseMultiplicativeDummy
}

// CHECK-AST-LABEL: internal struct TestPointwiseMultiplicative : Differentiable {
// CHECK-AST:         var w: PointwiseMultiplicativeDummy
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: PointwiseMultiplicativeDummy
// CHECK-AST:         internal init(w: PointwiseMultiplicativeDummy, technicallyDifferentiable: PointwiseMultiplicativeDummy)
// CHECK-AST:         internal struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative
// CHECK-AST:           internal typealias TangentVector = TestPointwiseMultiplicative.TangentVector


struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: internal struct TestKeyPathIterable : Differentiable, KeyPathIterable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         internal struct TangentVector : Differentiable, AdditiveArithmetic, ElementaryFunctions, VectorProtocol, KeyPathIterable
// CHECK-AST:           internal typealias TangentVector = TestKeyPathIterable.TangentVector

struct GenericTanMember<T : Differentiable> : Differentiable, AdditiveArithmetic {
  var x: T.TangentVector
}

// CHECK-AST-LABEL: internal struct GenericTanMember<T> : Differentiable, AdditiveArithmetic where T : Differentiable
// CHECK-AST:   internal var x: T.TangentVector
// CHECK-AST:   internal init(x: T.TangentVector)
// CHECK-AST:   internal typealias TangentVector = GenericTanMember<T>
// CHECK-AST:   internal static var zero: GenericTanMember<T> { get }
// CHECK-AST:   internal static func + (lhs: GenericTanMember<T>, rhs: GenericTanMember<T>) -> GenericTanMember<T>
// CHECK-AST:   internal static func - (lhs: GenericTanMember<T>, rhs: GenericTanMember<T>) -> GenericTanMember<T>
// CHECK-AST:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: GenericTanMember<T>, _ b: GenericTanMember<T>) -> Bool

public struct ConditionallyDifferentiable<T> {
  public var x: T
}
extension ConditionallyDifferentiable : Differentiable where T : Differentiable {}

// CHECK-AST-LABEL: public struct ConditionallyDifferentiable<T> {
// CHECK-AST:         @differentiable(wrt: self where T : Differentiable)
// CHECK-AST:         public var x: T
// CHECK-AST:         internal init(x: T)
// CHECK-AST:       }

// Verify that `TangentVector` is not synthesized to be `Self` for
// `AdditiveArithmetic`-conforming classes.
final class AdditiveArithmeticClass<T : AdditiveArithmetic & Differentiable> : AdditiveArithmetic, Differentiable {
  var x, y: T
  init(x: T, y: T) {
    self.x = x
    self.y = y
  }

  // Dummy `AdditiveArithmetic` requirements.
  static func == (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass) -> Bool {
    fatalError()
  }
  static var zero: AdditiveArithmeticClass {
    fatalError()
  }
  static func + (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass) -> Self {
    fatalError()
  }
  static func - (lhs: AdditiveArithmeticClass, rhs: AdditiveArithmeticClass) -> Self {
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
// CHECK-AST:   @frozen public struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {

@usableFromInline
struct UsableFromInlineStruct: Differentiable {}

// CHECK-AST-LABEL: @usableFromInline
// CHECK-AST: struct UsableFromInlineStruct : Differentiable {
// CHECK-AST:   internal init()
// CHECK-AST:   @usableFromInline
// CHECK-AST:   struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
