// RUN: %target-swift-frontend -typecheck -verify %s

import StdlibUnittest

var ProtocolRequirementAutodiffTests = TestSuite("ProtocolRequirementAutodiff")

// MARK: - Func requirements.

protocol DiffReq : Differentiable {
  @differentiable(wrt: (self, x))
  func f(_ x: Float) -> Float
}

extension DiffReq where TangentVector : AdditiveArithmetic {
  @inline(never)  // Prevent specialization, to test all witness code.
  func gradF(at x: Float) -> (Self.TangentVector, Float) {
    return (valueWithPullback(at: x) { s, x in s.f(x) }).1(1)
  }
}

struct Quadratic : DiffReq, Equatable {
  typealias TangentVector = Quadratic

  @differentiable
  let a: Float

  @differentiable
  let b: Float

  @differentiable
  let c: Float

  init(_ a: Float, _ b: Float, _ c: Float) {
    self.a = a
    self.b = b
    self.c = c
  }

  @differentiable(wrt: (self, x))
  func f(_ x: Float) -> Float {
    return a * x * x + b * x + c
  }
}

extension Quadratic : VectorProtocol {
  static var zero: Quadratic { return Quadratic(0, 0, 0) }
  static func + (lhs: Quadratic, rhs: Quadratic) -> Quadratic {
    return Quadratic(lhs.a + rhs.a, lhs.b + rhs.b, lhs.c + rhs.c)
  }
  static func - (lhs: Quadratic, rhs: Quadratic) -> Quadratic {
  return Quadratic(lhs.a + rhs.a, lhs.b + rhs.b, lhs.c + rhs.c)
}
  typealias Scalar = Float
  static func * (lhs: Float, rhs: Quadratic) -> Quadratic {
    return Quadratic(lhs * rhs.a, lhs * rhs.b, lhs * rhs.c)
  }
}

ProtocolRequirementAutodiffTests.test("func") {
  expectEqual((Quadratic(0, 0, 1), 12), Quadratic(11, 12, 13).gradF(at: 0))
  expectEqual((Quadratic(1, 1, 1), 2 * 11 + 12),
              Quadratic(11, 12, 13).gradF(at: 1))
  expectEqual((Quadratic(4, 2, 1), 2 * 11 * 2 + 12),
              Quadratic(11, 12, 13).gradF(at: 2))
}

// MARK: Constructor, accessor, and subscript requirements.

protocol FunctionsOfX: Differentiable {
  @differentiable
  init(x: Float)

  @differentiable
  var x: Float { get }

  @differentiable
  var y: Float { get }

  @differentiable
  var z: Float { get }

  @differentiable
  subscript() -> Float { get }
}

struct TestFunctionsOfX: FunctionsOfX {
  @differentiable
  init(x: Float) {
    self.x = x
    self.y = x * x
  }

  /// x = x
  var x: Float

  /// y = x * x
  var y: Float

  /// z = x * x + x
  var z: Float {
    return y + x
  }

  @differentiable
  subscript() -> Float {
    return z
  }
}

@inline(never)  // Prevent specialization, to test all witness code.
func derivatives<F: FunctionsOfX>(at x: Float, in: F.Type)
  -> (Float, Float, Float, Float)
{
  let dxdx = gradient(at: x) { x in F(x: x).x }
  let dydx = gradient(at: x) { x in F(x: x).y }
  let dzdx = gradient(at: x) { x in F(x: x).z }
  let dsubscriptdx = gradient(at: x) { x in F(x: x)[] }
  return (dxdx, dydx, dzdx, dsubscriptdx)
}

ProtocolRequirementAutodiffTests.test("constructor, accessor, subscript") {
  expectEqual(
    derivatives(at: 2.0, in: TestFunctionsOfX.self),
    (1.0, 4.0, 5.0, 5.0))
}

// MARK: - Test witness method SIL type computation.

protocol P : Differentiable {
  @differentiable(wrt: (x, y))
  func foo(_ x: Float, _ y: Double) -> Float
}
struct S : P {
  @differentiable(wrt: (x, y))
  func foo(_ x: Float, _ y: Double) -> Float {
    return x
  }
}

// MARK: - Differentiable protocol requirement with 'where' clause.

// Missing `@differentiable` attribute, without printing the 'wrt' arguments.
protocol DifferentiableWhereClause: Differentiable {
  associatedtype Scalar

  @differentiable(where Scalar: Differentiable) // expected-error {{'where' clauses in '@differentiable' attributes of protocol requirements are not supported}}
  func test(value: Scalar) -> Float
}

// MARK: - Overridden protocol method adding differentiable attribute.

public protocol Distribution {
  associatedtype Value
  func logProbability(of value: Value) -> Float
}

public protocol DifferentiableDistribution: Differentiable, Distribution {
  @differentiable(wrt: self)
  func logProbability(of value: Value) -> Float
}

struct Foo: DifferentiableDistribution {
  @differentiable(wrt: self)
  func logProbability(of value: Float) -> Float {
    .zero
  }
}

@differentiable
func blah<T: DifferentiableDistribution>(_ x: T) -> Float where T.Value: AdditiveArithmetic {
  x.logProbability(of: .zero)
}

// Adding a more general `@differentiable` attribute.
public protocol DoubleDifferentiableDistribution: DifferentiableDistribution
  where Value: Differentiable {
  @differentiable(wrt: self)
  @differentiable(wrt: (self, value))
  func logProbability(of value: Value) -> Float
}

@differentiable
func blah2<T: DoubleDifferentiableDistribution>(_ x: T, _ value: T.Value) -> Float
  where T.Value: AdditiveArithmetic {
  x.logProbability(of: value)
}

// Missing a `@differentiable` attribute.
public protocol MissingDifferentiableDistribution: DifferentiableDistribution
  where Value: Differentiable {
  func logProbability(of value: Value) -> Float // expected-note {{candidate is missing attribute '@differentiable(wrt: self)'}}
}

// Missing `@differentiable` attribute, without printing the 'wrt' arguments.
protocol Example: Differentiable {
  associatedtype Scalar: Differentiable

  @differentiable
  func test(value: Scalar) -> Float
}

protocol MissingDifferentiableTest: Example {
  func test(value: Scalar) -> Float // expected-note {{candidate is missing attribute '@differentiable'}}
}

runAllTests()
