// RUN: %empty-directory(%t)

// RUNx: %target-swift-frontend-dump-parse \
// RUNx:   -enable-experimental-feature ParserASTGen \
// RUNx:   | %sanitize-address > %t/astgen.ast

// RUNx: %target-swift-frontend-dump-parse \
// RUNx:   | %sanitize-address > %t/cpp-parser.ast

// RUNx: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift

import _Differentiation

func testDifferentiableTypeAttr(_ fn: @escaping @differentiable(reverse) (Float) -> Float)
    -> @differentiable(reverse) (Float) -> Float {
  return fn
}
func testDifferentiableTypeAttrLinear(_ fn: @escaping @differentiable(_linear) (Float) -> Float)
    -> @differentiable(_linear) (Float) -> Float {
  return fn
}

@differentiable(reverse)
func testDifferentiableSimple(_ x: Float) -> Float { return x * x }
@differentiable(reverse, wrt: arg1)
func testDifferentiableWRT1(arg1: Float, arg2: Float) -> Float { return arg1 }
@differentiable(reverse, wrt: (arg1, arg2))
func testDifferentiableWRT2(arg1: Float, arg2: Float) -> Float { return arg1 * arg2 }
@differentiable(reverse where T : Differentiable)
func testOnlyWhereClause<T : Numeric>(x: T) -> T { return x }
protocol DiffP {}
extension DiffP {
  @differentiable(reverse, wrt: self where Self : Differentiable)
  func testWhereClauseMethod() -> Self {
    return self
  }
}


func linearFunc(_ x: Float) -> Float { return x }
@transpose(of: linearFunc, wrt: 0)
func linearFuncTranspose(x: Float) -> Float { return x }

extension Float {
  func getDouble() -> Double { return Double(self) }

  @transpose(of: Float.getDouble, wrt: self)
  static func structTranspose(v: Double) -> Float { return Float(v) }
}

struct DerivativeTest<T: Differentiable & AdditiveArithmetic>: Differentiable,  AdditiveArithmetic  {

  typealias TangentVector = DerivativeTest<T.TangentVector>

  static var zero: Self {
    fatalError()
  }
  static func + (lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  static func - (lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  mutating func move(by offset: TangentVector) {
    x.move(by: offset.x)
  }

  var x: T

  static func staticMethod(_ x: Float) -> Float { 1.2 }
  @derivative(of: staticMethod)
  static func jvpStaticMethod(x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (x, { $0 })
  }

  func instanceMethod(_ x: T) -> T { x }
  @derivative(of: instanceMethod)
  func jvpInstanceMethod(x: T) -> (value: T, differential: (TangentVector, T.TangentVector) -> T.TangentVector) {
    return (x, { $1 })
  }

  init(_ x: Float) { fatalError() }
  init(_ x: T, y: Float) { fatalError() }

  @derivative(of: init(_:y:))
  static func vjpInit2(_ x: T, _ y: Float) -> (value: Self, pullback: (TangentVector) -> (T.TangentVector, Float)) {
    return (.init(x, y: y), { _ in (.zero, .zero) })
  }

  var computedProperty: T {
    get { x }
    set { x = newValue }
  }
  // FIXME: SwiftParser parsed this attribute as:
  //   {type: 'computedProperty', originalName: 'get', accessor: null}
  // But it should be:
  //   {type: null, originalName: 'computedProperty', accessor: 'get'}
  // @derivative(of: computedProperty.get)
  // func jvpProperty() -> (value: T, differential: (TangentVector) -> T.TangentVector) {
  //   fatalError()
  // }

  subscript(float float: Float) -> Float {
    get { 1 }
    set {}
  }
  @derivative(of: subscript(float:).get, wrt: self)
  func vjpSubscriptLabeledGetter(float: Float) -> (value: Float, pullback: (Float) -> TangentVector)  {
    return (1, { _ in .zero })
  }
}
