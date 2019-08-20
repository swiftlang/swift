// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardMode")

//===----------------------------------------------------------------------===//
// Basic tests.
//===----------------------------------------------------------------------===//

ForwardModeTests.test("Unary") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: func_to_diff)
  expectEqual(16, y)
  expectEqual(8, differential(1))
}

ForwardModeTests.test("Binary") {
  func func_to_diff(x: Float, y: Float) -> Float {
    return x * y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: func_to_diff)
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("BinaryWithLets") {
  func func_to_diff(x: Float, y: Float) -> Float {
    let a = x + y
    let b = a
    return b * -y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: func_to_diff)
  expectEqual(-45, y)
  expectEqual(-19, differential(1, 1))
}

//===----------------------------------------------------------------------===//
// `Tracked` struct 
//===----------------------------------------------------------------------===//

ForwardModeTests.test("TrackedIdentity") {
  func identity(x: Tracked<Float>) -> Tracked<Float> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: identity)
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

ForwardModeTests.test("TrackedAddition") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x + y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(9, y)
  expectEqual(2, differential(1, 1))
}

ForwardModeTests.test("TrackedDivision") {
  func divide(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x / y
  }
  let (y, differential) = valueWithDifferential(at: 10, 5, in: divide)
  expectEqual(2, y)
  expectEqual(-0.2, differential(1, 1))
}

ForwardModeTests.test("TrackedMultipleMultiplication") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    return x * y * x
  }
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(80, y)
  // 2yx+xx
  expectEqual(56, differential(1, 1))
}

ForwardModeTests.test("TrackedWithLets") {
  func add(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    let a = x + y
    let b = a * a // (x+y)^2
    let c = b / x + y // (x+y)^2/x+y
    return c
  }
  // (3x^2+2xy-y^2)/x^2+1
  let (y, differential) = valueWithDifferential(at: 4, 5, in: add)
  expectEqual(25.25, y)
  expectEqual(4.9375, differential(1, 1))
}

ForwardModeTests.test("TrackedDifferentiableFuncType") {
  func valAndDeriv(
    f: @escaping @differentiable (Tracked<Float>) -> Tracked<Float>
  ) -> (Tracked<Float>, Tracked<Float>) {
    let (y, diff) = valueWithDifferential(at: 5, in: f)
    return (y, diff(1))
  }

  func func1(_ x: Tracked<Float>) -> Tracked<Float> {
    let a = x + x // 2x
    let b = a + a // 4x
    return b * b // 16x^2
  }
  let (val1, dv1) = valAndDeriv(f: func1)
  expectEqual(val1, 400)
  expectEqual(dv1, 160)
}
//===----------------------------------------------------------------------===//
// Classes
//===----------------------------------------------------------------------===//
// NOTE: once forward mode is done, can copy and replace this in 
// `class_method.swift` as it already calls reverse mode functions.

ForwardModeTests.test("Final") {
  final class Final : Differentiable {
    func method(_ x: Float) -> Float {
      return x * x
    }
  }

  for i in -5...5 {
    expectEqual(Float(i) * 2, gradient(at: Float(i)) { x in Final().method(x) })
    expectEqual(
      Float(i) * 2, 
      derivative(at: Float(i)) { x in Final().method(x) })
  }
}

ForwardModeTests.test("Simple") {
  class Super {
    @differentiable(wrt: x, jvp: jvpf, vjp: vjpf)
    func f(_ x: Float) -> Float {
      return 2 * x
    }
    final func jvpf(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 2 * v })
    }
    final func vjpf(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 2 * v })
    }
  }

  class SubOverride : Super {
    @differentiable(wrt: x)
    override func f(_ x: Float) -> Float {
      return 3 * x
    }
  }

  class SubOverrideCustomDerivatives : Super {
    @differentiable(wrt: x, jvp: jvpf2, vjp: vjpf2)
    override func f(_ x: Float) -> Float {
      return 3 * x
    }
    final func jvpf2(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 3 * v })
    }
    final func vjpf2(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 3 * v })
    }
  }

  func classValueWithDerivative(_ c: Super) -> (Float, Float) {
    return valueWithDerivative(at: 1) { c.f($0) }
  }
  func classValueWithGradient(_ c: Super) -> (Float, Float) {
    return valueWithGradient(at: 1) { c.f($0) }
  }

  expectEqual((2, 2), classValueWithDerivative(Super()))
  expectEqual((3, 3), classValueWithDerivative(SubOverride()))
  expectEqual((3, 3), classValueWithDerivative(SubOverrideCustomDerivatives()))
  expectEqual((2, 2), classValueWithGradient(Super()))
  expectEqual((3, 3), classValueWithGradient(SubOverride()))
  expectEqual((3, 3), classValueWithGradient(SubOverrideCustomDerivatives()))
}

ForwardModeTests.test("SimpleWrtSelf") {
  class Super : Differentiable {
    var base: Float
    // FIXME(TF-648): Dummy to make `Super.AllDifferentiableVariables` be nontrivial.
    var _nontrivial: [Float] = []

    // TODO(TF-654): Uncomment attribute when differentiation supports class initializers.
    // TODO(TF-645): Remove `vjpInit` when differentiation supports `ref_element_addr`.
    // @differentiable(vjp: vjpInit)
    required init(base: Float) {
      self.base = base
    }
    static func vjpInit(base: Float) -> (Super, (TangentVector) -> Float) {
      return (Super(base: base), { x in x.base })
    }

    static func jvpInit(base: Float) -> (Super, (Float) -> TangentVector) {
      return (Super(base: base), { x in TangentVector(base: x, _nontrivial: []) })
    }

    @differentiable(wrt: (self, x), jvp: jvpf, vjp: vjpf)
    func f(_ x: Float) -> Float {
      return base * x
    }
    final func jvpf(_ x: Float) -> (Float, (TangentVector, Float) -> Float) {
      return (f(x), { (dself, dx) in dself.base * dx })
    }
    final func vjpf(_ x: Float) -> (Float, (Float) -> (TangentVector, Float)) {
      let base = self.base
      return (f(x), { v in
        (TangentVector(base: v * x, _nontrivial: []), base * v)
      })
    }
  }

  class SubOverride : Super {
    @differentiable(wrt: (self, x))
    override func f(_ x: Float) -> Float {
      return 3 * x
    }
  }

  class SubOverrideCustomDerivatives : Super {
    @differentiable(wrt: (self, x))
    @differentiable(wrt: x, jvp: jvpf2, vjp: vjpf2)
    override func f(_ x: Float) -> Float {
      return 3 * x
    }
    final func jvpf2(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 3 * v })
    }
    final func vjpf2(_ x: Float) -> (Float, (Float) -> Float) {
      return (f(x), { v in 3 * v })
    }
  }

  // TODO(TF-654): Uncomment when differentiation supports class initializers.
  // let v = Super.TangentVector(base: 100, _nontrivial: [])
  // expectEqual(100, pullback(at: 1337) { x in Super(base: x) }(v))
  // expectEqual(100, pullback(at: 1337) { x in SubOverride(base: x) }(v))
  // expectEqual(100, pullback(at: 1337) { x in SubOverrideCustomDerivatives(base: x) }(v))
  

  // `valueWithGradient` is not used because nested tuples cannot be compared
  // with `expectEqual`.
  func classGradient(_ c: Super) -> (Super.TangentVector, Float) {
    return gradient(at: c, 10) { c, x in c.f(x) }
  }

  // `valueWithDerivative` is not used because the derivative requires `Super`
  // to conform to `FloatingPoint`.
  func classDifferential(
    _ c: Super
  ) -> (Float, (Super.TangentVector, Float) -> Float) {
    return valueWithDifferential(at: c, 10) { (c: Super, x: Float) in c.f(x) }
  }

  let (y1, diff1) = classDifferential(Super(base: 5))
  expectEqual(y1, 50)
  expectEqual(diff1(Super.TangentVector(base: 1, _nontrivial: []), 1), 1)
  let (y2, diff2) = classDifferential(SubOverride(base: 5))
  expectEqual(y2, 30)
  expectEqual(diff2(SubOverride.TangentVector(base: 1, _nontrivial: []), 1), 3)
  let (y3, diff3) = classDifferential(SubOverrideCustomDerivatives(base: 5))
  expectEqual(y3, 30)
  expectEqual(diff3(SubOverrideCustomDerivatives
                      .TangentVector(base: 1, _nontrivial: []), 1), 3)
  expectEqual((Super.TangentVector(base: 10, _nontrivial: []), 2),
              classGradient(Super(base: 2)))
  expectEqual((Super.TangentVector(base: 0, _nontrivial: []), 3),
              classGradient(SubOverride(base: 2)))
  expectEqual((Super.TangentVector(base: 0, _nontrivial: []), 3),
              classGradient(SubOverrideCustomDerivatives(base: 2)))
}

//===----------------------------------------------------------------------===//
// Protocols
//===----------------------------------------------------------------------===//
// TODO: add more protocol tests.
protocol DiffReq : Differentiable {
  func foo(x: Float) -> Float
}

struct Linear: DiffReq, VectorProtocol {
  typealias TangentVector = Linear

  @differentiable
  let m: Float

  @differentiable
  let b: Float

  @differentiable
  func foo(x: Float) -> Float {
    return m * x + b
  }
}

ForwardModeTests.test("Protocols") {
  let inst = Linear(m: 5, b: -2)
  let (y1, diff1) = valueWithDifferential(at: 5) { x in inst.foo(x: x) }
  expectEqual(y1, 23)
  expectEqual(diff1(1), 5)
}

runAllTests()
