// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var ProtocolRequirementAutodiffTests = TestSuite("ProtocolRequirementAutodiff")

// MARK: - Func requirements.

protocol DiffReq : Differentiable {
  @differentiable(wrt: (self, x))
  func f(_ x: Tracked<Float>) -> Tracked<Float>
}

extension DiffReq where TangentVector : AdditiveArithmetic {
  @inline(never)  // Prevent specialization, to test all witness code.
  func gradF(at x: Tracked<Float>) -> (Self.TangentVector, Tracked<Float>) {
    return (valueWithPullback(at: self, x) { s, x in s.f(x) }).1(1)
  }
}

struct Quadratic : DiffReq, AdditiveArithmetic {
  typealias TangentVector = Quadratic

  @differentiable
  let a: Tracked<Float>

  @differentiable
  let b: Tracked<Float>

  @differentiable
  let c: Tracked<Float>

  init(_ a: Tracked<Float>, _ b: Tracked<Float>, _ c: Tracked<Float>) {
    self.a = a
    self.b = b
    self.c = c
  }

  @differentiable(wrt: (self, x))
  func f(_ x: Tracked<Float>) -> Tracked<Float> {
    return a * x * x + b * x + c
  }
}

ProtocolRequirementAutodiffTests.testWithLeakChecking("func") {
  expectEqual((Quadratic(0, 0, 1), 12), Quadratic(11, 12, 13).gradF(at: 0))
  expectEqual((Quadratic(1, 1, 1), 2 * 11 + 12),
              Quadratic(11, 12, 13).gradF(at: 1))
  expectEqual((Quadratic(4, 2, 1), 2 * 11 * 2 + 12),
              Quadratic(11, 12, 13).gradF(at: 2))
}

// MARK: Constructor, accessor, and subscript requirements.

protocol FunctionsOfX: Differentiable {
  @differentiable
  init(x: Tracked<Float>)

  @differentiable
  var x: Tracked<Float> { get }

  @differentiable
  var y: Tracked<Float> { get }

  @differentiable
  var z: Tracked<Float> { get }

  @differentiable
  subscript() -> Tracked<Float> { get }
}

struct TestFunctionsOfX: FunctionsOfX {
  @differentiable
  init(x: Tracked<Float>) {
    self.x = x
    self.y = x * x
  }

  /// x = x
  var x: Tracked<Float>

  /// y = x * x
  var y: Tracked<Float>

  /// z = x * x + x
  var z: Tracked<Float> {
    return y + x
  }

  @differentiable
  subscript() -> Tracked<Float> {
    return z
  }
}

@inline(never)  // Prevent specialization, to test all witness code.
func derivatives<F: FunctionsOfX>(at x: Tracked<Float>, in: F.Type)
  -> (Tracked<Float>, Tracked<Float>, Tracked<Float>, Tracked<Float>)
{
  let dxdx = gradient(at: x) { x in F(x: x).x }
  let dydx = gradient(at: x) { x in F(x: x).y }
  let dzdx = gradient(at: x) { x in F(x: x).z }
  let dsubscriptdx = gradient(at: x) { x in F(x: x)[] }
  return (dxdx, dydx, dzdx, dsubscriptdx)
}

ProtocolRequirementAutodiffTests.testWithLeakChecking("constructor, accessor, subscript") {
  expectEqual(
    (1.0, 4.0, 5.0, 5.0),
    derivatives(at: 2.0, in: TestFunctionsOfX.self))
}

// MARK: - Test witness method SIL type computation.

protocol P : Differentiable {
  @differentiable(wrt: (x, y))
  func foo(_ x: Tracked<Float>, _ y: Double) -> Tracked<Float>
}
struct S : P {
  @differentiable(wrt: (x, y))
  func foo(_ x: Tracked<Float>, _ y: Double) -> Tracked<Float> {
    return x
  }
}

// MARK: - Overridden protocol method adding differentiable attribute.

public protocol Distribution {
  associatedtype Value
  func logProbability(of value: Value) -> Tracked<Float>
}

public protocol DifferentiableDistribution: Differentiable, Distribution {
  @differentiable(wrt: self)
  func logProbability(of value: Value) -> Tracked<Float>
}

struct Foo: DifferentiableDistribution {
  @differentiable(wrt: self)
  func logProbability(of value: Tracked<Float>) -> Tracked<Float> {
    .zero
  }
}

@differentiable
func blah<T: DifferentiableDistribution>(_ x: T) -> Tracked<Float>
where T.Value: AdditiveArithmetic {
  x.logProbability(of: .zero)
}

// Adding a more general `@differentiable` attribute.
public protocol DoubleDifferentiableDistribution: DifferentiableDistribution
  where Value: Differentiable {
  @differentiable(wrt: self)
  @differentiable(wrt: (self, value))
  func logProbability(of value: Value) -> Tracked<Float>
}

@differentiable
func blah2<T: DoubleDifferentiableDistribution>(_ x: T, _ value: T.Value) -> Tracked<Float>
  where T.Value: AdditiveArithmetic {
  x.logProbability(of: value)
}

// Satisfying the requirement with more wrt indices than are necessary.

protocol DifferentiableFoo {
  associatedtype T: Differentiable
  @differentiable(wrt: x)
  func foo(_ x: T) -> Tracked<Float>
}

protocol MoreDifferentiableFoo: Differentiable, DifferentiableFoo {
  @differentiable(wrt: (self, x))
  func foo(_ x: T) -> Tracked<Float>
}

struct MoreDifferentiableFooStruct: MoreDifferentiableFoo {
  @differentiable(wrt: (self, x))
  func foo(_ x: Tracked<Float>) -> Tracked<Float> {
    x
  }
}

// Satisfiying the requirement with a less-constrained derivative than is necessary.

protocol ExtraDerivativeConstraint {}

protocol HasExtraConstrainedDerivative {
  @differentiable
  func requirement<T: Differentiable & ExtraDerivativeConstraint>(_ x: T) -> T
}

struct SatisfiesDerivativeWithLessConstraint: HasExtraConstrainedDerivative {
  @differentiable
  func requirement<T: Differentiable>(_ x: T) -> T { x }
}

runAllTests()
