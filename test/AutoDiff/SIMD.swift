// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SIMDTests = TestSuite("SIMD")

SIMDTests.test("Addition") {
  let a = SIMD4<Float>(1,2,3,4)

  let foo1 = { (x: SIMD4<Float>) -> SIMD4<Float> in
    return x
  }
  let bp1 = pullback(at: a, in: foo1)
  expectEqual(a, bp1(a))

  let foo2 = { (x: SIMD4<Float>, y: SIMD4<Float>) -> SIMD4<Float> in
    return x + y
  }
  let bp2 = pullback(at: a, a, in: foo2)
  expectEqual((a, a), bp2(a))
  
  let foo3 = { (x: SIMD4<Float>, y: Float) -> SIMD4<Float> in
    return -x * y
  }
  let bp3 = pullback(at: a, 5, in: foo3)
  expectEqual((a, 1), bp3(a))
  
  let foo4 = { (x: Float, y: SIMD4<Float>) -> SIMD4<Float> in
    return -x + y
  }
  let bp4 = pullback(at: 5, a, in: foo4)
  expectEqual((1, a), bp4(a))
}

SIMDTests.test("Fanout") {
  let foo1 = { (x: Float) -> Float in
     x - x
  }
  expectEqual(0, gradient(at: 100, in: foo1))
  let foo2 = { (x: Float) -> Float in
     x + x
  }
  expectEqual(2, gradient(at: 100, in: foo2))
  let foo3 = { (x: Float, y: Float) -> Float in
    x + x + x * y
  }
  expectEqual((4, 3), gradient(at: 3, 2, in: foo3))
}

SIMDTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return 3 * x + { $0 * 3 }(3) * y
  }
  expectEqual((3, 9), gradient(at: 3, 4, in: foo))
  expectEqual(3, gradient(at: 3) { x in foo(x, 4) })
}

SIMDTests.test("ResultSelection") {
  func foo(_ x: Float, _ y: Float) -> (Float, Float) {
    return (x + 1, y + 2)
  }
  expectEqual((1, 0), gradient(at: 3, 3, in: { x, y in foo(x, y).0 }))
  expectEqual((0, 1), gradient(at: 3, 3, in: { x, y in foo(x, y).1 }))
}

SIMDTests.test("CaptureLocal") {
  let z: Float = 10
  func foo(_ x: Float) -> Float {
    return z * x
  }
  expectEqual(10, gradient(at: 0, in: foo))
}

var globalVar: Float = 10
SIMDTests.test("CaptureGlobal") {
  let foo: (Float) -> Float = { x in
    globalVar += 20
    return globalVar * x
  }
  expectEqual(30, gradient(at: 0, in: foo))
}

let foo: (Float) -> Float = { x in
  return x * x
}
SIMDTests.test("GlobalLet") {
  expectEqual(2, gradient(at: 1, in: foo))
}

var foo_diffable: @differentiable (Float) -> (Float)
  = differentiableFunction { x in (x * x, { v in 2 * x * v }) }
SIMDTests.test("GlobalDiffableFunc") {
  expectEqual(2, gradient(at: 1, in: foo_diffable))
  expectEqual(2, gradient(at: 1, in: { x in foo_diffable(x) }))
  expectEqual(1, gradient(at: 1, in: { (x: Float) -> Float in
    foo_diffable = { x in x + 1 };
    return foo_diffable(x)
  }))
  expectEqual(1, gradient(at: 1, in: foo_diffable))
}

SIMDTests.test("SideEffects") {
  func fourthPower(x: Float) -> Float {
    var a = x
    a = a * x
    a = a * x
    return a * x
  }
  expectEqual(4 * 27, gradient(at: 3, in: fourthPower))
}

SIMDTests.test("TupleSideEffects") {
  func foo(_ x: Float) -> Float {
    var tuple = (x, x)
    tuple.0 = tuple.0 * x
    return x * tuple.0
  }
  expectEqual(27, gradient(at: 3, in: foo))

  func fifthPower(_ x: Float) -> Float {
    var tuple = (x, x)
    tuple.0 = tuple.0 * x
    tuple.1 = tuple.0 * x
    return tuple.0 * tuple.1
  }
  expectEqual(405, gradient(at: 3, in: fifthPower))

  func nested(_ x: Float) -> Float {
    var tuple = ((x, x), x)
    tuple.0.0 = tuple.0.0 * x
    tuple.0.1 = tuple.0.0 * x
    return tuple.0.0 * tuple.0.1
  }
  expectEqual(405, gradient(at: 3, in: nested))
}

// Tests TF-321.
SIMDTests.test("TupleNonDifferentiableElements") {
  func foo(_ x: Float) -> Float {
    var tuple = (x, 1)
    tuple.0 = x
    tuple.1 = 1
    return tuple.0
  }
  expectEqual(1, gradient(at: 1, in: foo))

  func bar(_ x: Float) -> Float {
    var tuple: (Int, Int, Float, Float) = (1, 1, x, x)
    tuple.0 = 1
    tuple.1 = 1
    tuple.3 = x
    return tuple.3
  }
  expectEqual(1, gradient(at: 1, in: bar))

  struct Wrapper<T> {
    @differentiable(where T : Differentiable)
    func baz(_ x: T) -> T {
      var tuple = (1, 1, x, 1)
      tuple.0 = 1
      tuple.2 = x
      tuple.3 = 1
      return tuple.2
    }
  }
  expectEqual(1, gradient(at: Float(1), in: { x -> Float in
    let wrapper = Wrapper<Float>()
    return wrapper.baz(x)
  }))
}

// Tests TF-21.
SIMDTests.test("StructMemberwiseInitializer") {
  struct Foo : AdditiveArithmetic, Differentiable {
    var stored: Float
    var computed: Float {
      return stored * stored
    }
  }

  let 𝛁foo = pullback(at: Float(4), in: { input -> Foo in
    let foo = Foo(stored: input)
    return foo + foo
  })(Foo.TangentVector(stored: 1))
  expectEqual(2, 𝛁foo)

  let 𝛁computed = gradient(at: Float(4)) { input -> Float in
    let foo = Foo(stored: input)
    return foo.computed
  }
  expectEqual(8, 𝛁computed)

  let 𝛁product = gradient(at: Float(4)) { input -> Float in
    let foo = Foo(stored: input)
    return foo.computed * foo.stored
  }
  expectEqual(16, 𝛁product)

  struct Custom : AdditiveArithmetic, Differentiable {
    var x: Float

    // Custom initializer with `@differentiable`.
    @differentiable
    init(x: Float) {
      print(x)
      self.x = x
    }
  }

  let 𝛁custom = pullback(at: Float(4), in: { input -> Custom in
    let foo = Custom(x: input)
    return foo + foo
  })(Custom.TangentVector(x: 1))
  expectEqual(2, 𝛁custom)
}

// Tests TF-319: struct with non-differentiable constant stored property.
SIMDTests.test("StructConstantStoredProperty") {
  struct TF_319 : Differentiable {
    var x: Float
    @noDerivative let constant = Float(2)

    @differentiable
    init(x: Float) {
      self.x = x
    }

    @differentiable(wrt: (self, input))
    func applied(to input: Float) -> Float {
      return x * constant * input
    }
  }
  func testStructInit(to input: Float) -> Float {
    let model = TF_319(x: 10)
    return model.applied(to: input)
  }
  expectEqual(TF_319.TangentVector(x: 6),
              gradient(at: TF_319(x: 10), in: { $0.applied(to: 3) }))
  expectEqual(20, gradient(at: 3, in: testStructInit))
}

SIMDTests.test("StructSideEffects") {
  struct Point : AdditiveArithmetic, Differentiable {
    var x: Float
    var y: Float
    var z: Float
  }

  func double(_ input: Float) -> Point {
    let point = Point(x: input, y: input, z: input)
    return point + point
  }
  expectEqual(6, pullback(at: 4, in: double)(Point(x: 1, y: 1, z: 1)))

  func fifthPower(_ input: Float) -> Float {
    var point = Point(x: input, y: input, z: input)
    point.x = point.x * input
    point.y = point.x * input
    return point.x * point.y
  }
  expectEqual(405, gradient(at: 3, in: fifthPower))

  func mix(_ input: Float) -> Float {
    var tuple = (point: Point(x: input, y: input, z: input), float: input)
    tuple.point.x = tuple.point.x * tuple.float
    tuple.point.y = tuple.point.x * input
    return tuple.point.x * tuple.point.y
  }
  expectEqual(405, gradient(at: 3, in: mix))

  // Test TF-282.
  struct Add : Differentiable {
    var bias: Float
    func applied(to input: Float) -> Float {
      var tmp = input
      tmp = tmp + bias
      return tmp
    }
  }
  let model = Add(bias: 1)
  expectEqual(Add.TangentVector(bias: 1), gradient(at: model) { m in m.applied(to: 1) })
}

SIMDTests.test("StructGeneric") {
  struct Generic<T : AdditiveArithmetic & Differentiable> : AdditiveArithmetic, Differentiable {
    var x: T
    var y: T
    var z: T
  }

  let 𝛁generic = pullback(at: Float(3), in: { input -> Generic<Float> in
    var generic = Generic(x: input, y: input, z: input)
    return generic
  })(Generic<Float>.TangentVector(x: 1, y: 1, z: 1))
  expectEqual(3, 𝛁generic)

  func fifthPower(_ input: Float) -> Float {
    var generic = Generic(x: input, y: input, z: input)
    generic.x = generic.x * input
    generic.y = generic.x * input
    return generic.x * generic.y
  }
  // FIXME(TF-274): The true expected result is `405`, like other variants of `fifthPower` above.
  expectEqual(405, gradient(at: 3, in: fifthPower))
}

SIMDTests.test("SubsetIndices") {
  func grad(_ lossFunction: @differentiable (Float, Float) -> Float) -> Float {
    return gradient(at: 1) { x in lossFunction(x * x, 10.0) }
  }
  expectEqual(2, grad { x, y in x + y })

  func gradWRTNonDiff(_ lossFunction: @differentiable (Float, @nondiff Int) -> Float) -> Float {
    return gradient(at: 2) { x in lossFunction(x * x, 10) }
  }
  expectEqual(4, gradWRTNonDiff { x, y in x + Float(y) })
}

runAllTests()
