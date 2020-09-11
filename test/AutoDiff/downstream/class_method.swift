// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ClassMethodTests = TestSuite("ClassMethods")

ClassMethodTests.test("Final") {
  final class Final: Differentiable {
    func method(_ x: Tracked<Float>) -> Tracked<Float> {
      return x * x
    }
  }

  for i in -5...5 {
  expectEqual(Tracked<Float>(Float(i * 2)),
              gradient(at: Tracked<Float>(Float(i))) {
                x in Final().method(x)
              })
  }
}

ClassMethodTests.test("Simple") {
  class Super {
    @differentiable(wrt: x)
    func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 2 * x
    }

    @derivative(of: f)
    final func jvpf(_ x: Tracked<Float>) -> (value: Tracked<Float>, differential: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 2 * v })
    }

    @derivative(of: f)
    final func vjpf(_ x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 2 * v })
    }
  }

  class SubOverride: Super {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 3 * x
    }
  }

  class SubOverrideCustomDerivatives: Super {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 3 * x
    }

    @derivative(of: f)
    final func jvpf2(_ x: Tracked<Float>) -> (value: Tracked<Float>, differential: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 3 * v })
    }

    @derivative(of: f)
    final func vjpf2(_ x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 3 * v })
    }
  }

  func classValueWithGradient(_ c: Super) -> (Tracked<Float>, Tracked<Float>) {
    return valueWithGradient(at: 1) { c.f($0) }
  }
  expectEqual((2, 2), classValueWithGradient(Super()))
  expectEqual((3, 3), classValueWithGradient(SubOverride()))
  expectEqual((3, 3), classValueWithGradient(SubOverrideCustomDerivatives()))
}

ClassMethodTests.test("SimpleWrtSelf") {
  class Super: Differentiable {
    var base: Tracked<Float>
    // FIXME(TF-648): Dummy to make `Super.AllDifferentiableVariables` be nontrivial.
    var _nontrivial: [Tracked<Float>] = []

    init(base: Tracked<Float>) {
      self.base = base
    }

    @differentiable(wrt: (self, x))
    func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return base * x
    }

    @derivative(of: f)
    final func jvpf(
      _ x: Tracked<Float>
    ) -> (value: Tracked<Float>, differential: (TangentVector, Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { (dself, dx) in dself.base * dx })
    }

    @derivative(of: f)
    final func vjpf(
      _ x: Tracked<Float>
    ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (TangentVector, Tracked<Float>)) {
      let base = self.base
      return (f(x), { v in
        (TangentVector(base: v * x, _nontrivial: []), base * v)
      })
    }
  }

  final class SubOverride: Super {
    @differentiable
    override init(base: Tracked<Float>) {
      super.init(base: base)
    }

    // Note: `TangentVector` type is unused.
    // There is no way to customize `SubOverride: Differentiable` conformance.
    // The conformance is always inherited from `Super`.
    struct TangentVector: Differentiable & AdditiveArithmetic {
      var base: Float
    }

    @differentiable(wrt: (self, x))
    override func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 3 * x
    }
  }

  final class SubOverrideCustomDerivatives: Super {
    @differentiable
    override init(base: Tracked<Float>) {
      super.init(base: base)
    }
    @derivative(of: init)
    static func vjpInit(base: Tracked<Float>) -> (
      value: SubOverrideCustomDerivatives, pullback: (Super.TangentVector) -> Tracked<Float>
    ) {
      return (SubOverrideCustomDerivatives(base: base), { x in x.base * 2 })
    }

    @differentiable(wrt: (self, x))
    @differentiable(wrt: x)
    override func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 3 * x
    }
    @derivative(of: f, wrt: x)
    final func jvpf2(_ x: Tracked<Float>) -> (value: Tracked<Float>, differential: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 3 * v })
    }
    @derivative(of: f, wrt: x)
    final func vjpf2(_ x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
      return (f(x), { v in 3 * v })
    }
  }

  let v = Super.TangentVector(base: 100, _nontrivial: [])
  expectEqual(100, pullback(at: 1337) { x in Super(base: x) }(v))
  expectEqual(100, pullback(at: 1337) { x in SubOverride(base: x) }(v))
  expectEqual(200, pullback(at: 1337) { x in SubOverrideCustomDerivatives(base: x) }(v))

  // `valueWithGradient` is not used because nested tuples cannot be compared
  // with `expectEqual`.
  func classGradient(_ c: Super) -> (Super.TangentVector, Tracked<Float>) {
    return gradient(at: c, 10) { c, x in c.f(x) }
  }
  expectEqual((Super.TangentVector(base: 10, _nontrivial: []), 2),
              classGradient(Super(base: 2)))
  expectEqual((Super.TangentVector(base: 0, _nontrivial: []), 3),
              classGradient(SubOverride(base: 2)))
  expectEqual((Super.TangentVector(base: 0, _nontrivial: []), 3),
              classGradient(SubOverrideCustomDerivatives(base: 2)))
}

ClassMethodTests.test("Generics") {
  class Super<T: Differentiable & FloatingPoint> where T == T.TangentVector {
    @differentiable(wrt: x)
    func f(_ x: Tracked<T>) -> Tracked<T> {
      return Tracked<T>(2) * x
    }
    @derivative(of: f)
    final func jvpf(
      _ x: Tracked<T>
    ) -> (value: Tracked<T>, differential: (Tracked<T>.TangentVector) -> Tracked<T>.TangentVector) {
      return (f(x), { v in Tracked<T>(2) * v })
    }
    @derivative(of: f)
    final func vjpf(
      _ x: Tracked<T>
    ) -> (value: Tracked<T>, pullback: (Tracked<T>.TangentVector) -> Tracked<T>.TangentVector) {
      return (f(x), { v in Tracked<T>(2) * v })
    }
  }

  class SubOverride<T: Differentiable & FloatingPoint>: Super<T> where T == T.TangentVector {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<T>) -> Tracked<T> {
      return x
    }
  }

  class SubSpecializeOverride: Super<Float> {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<Float>) -> Tracked<Float> {
      return 3 * x
    }
  }

  class SubOverrideCustomDerivatives<T: Differentiable & FloatingPoint>: Super<T>
  where T == T.TangentVector {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<T>) -> Tracked<T> {
      return Tracked<T>(3) * x
    }
    @derivative(of: f)
    final func jvpf2(
      _ x: Tracked<T>
    ) -> (value: Tracked<T>, differential: (Tracked<T>.TangentVector) -> Tracked<T>.TangentVector) {
      return (f(x), { v in Tracked<T>(3) * v })
    }
    @derivative(of: f)
    final func vjpf2(
      _ x: Tracked<T>
    ) -> (value: Tracked<T>, pullback: (Tracked<T>.TangentVector) -> Tracked<T>.TangentVector) {
      return (f(x), { v in Tracked<T>(3) * v })
    }
  }

  class SubSpecializeOverrideCustomDerivatives: Super<Float80> {
    @differentiable(wrt: x)
    override func f(_ x: Tracked<Float80>) -> Tracked<Float80> {
      return 3 * x
    }
    @derivative(of: f)
    final func jvpf2(
      _ x: Tracked<Float80>
    ) -> (value: Tracked<Float80>, differential: (Tracked<Float80>) -> Tracked<Float80>) {
      return (f(x), { v in 3 * v })
    }
    @derivative(of: f)
    final func vjpf2(
      _ x: Tracked<Float80>
    ) -> (value: Tracked<Float80>, pullback: (Tracked<Float80>) -> Tracked<Float80>) {
      return (f(x), { v in 3 * v })
    }
  }

  func classValueWithGradient<T: Differentiable & FloatingPoint>(
    _ c: Super<T>
  ) -> (T, T) where T == T.TangentVector {
    let (x,y) =  valueWithGradient(at: Tracked<T>(1), in: {
        c.f($0) })
    return (x.value, y.value)
  }
  expectEqual((2, 2), classValueWithGradient(Super<Float>()))
  expectEqual((1, 1), classValueWithGradient(SubOverride<Float>()))
  expectEqual((3, 3), classValueWithGradient(SubSpecializeOverride()))
  expectEqual((3, 3), classValueWithGradient(SubOverrideCustomDerivatives<Float>()))
  expectEqual((3, 3), classValueWithGradient(SubSpecializeOverrideCustomDerivatives()))
}

ClassMethodTests.test("Methods") {
  class Super: Differentiable {
    var base: Tracked<Float>
    // Dummy to make `Super.AllDifferentiableVariables` be nontrivial.
    var _nontrivial: [Tracked<Float>] = []

    init(base: Tracked<Float>) {
      self.base = base
    }

    @differentiable
    func squared() -> Tracked<Float> { base * base }

    @derivative(of: squared)
    final func vjpSquared() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> TangentVector) {
      let base = self.base
      return (base * base, { v in
        TangentVector(base: 2 * base * v, _nontrivial: [])
      })
    }
  }

  class Sub1: Super {
    @differentiable
    override func squared() -> Tracked<Float> { base * base }
    @derivative(of: squared)
    final func vjpSquared2() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> TangentVector) {
      let base = self.base
      return (base * base, { v in
        TangentVector(base: 2 * base * v, _nontrivial: [])
      })
    }
  }

  func classValueWithGradient(_ c: Super) -> (Tracked<Float>, Super.TangentVector) {
    return valueWithGradient(at: c) { c in c.squared() }
  }

  expectEqual(4, gradient(at: 2) { x in Super(base: x).squared() })
  expectEqual(4, gradient(at: 2) { x in Sub1(base: x).squared() })

  expectEqual(Super.TangentVector(base: 4, _nontrivial: []),
              gradient(at: Super(base: 2)) { foo in foo.squared() })
  expectEqual(Sub1.TangentVector(base: 4, _nontrivial: []),
              gradient(at: Sub1(base: 2)) { foo in foo.squared() })
}

ClassMethodTests.test("Properties") {
  class Super: Differentiable {
    @differentiable
    var base: Tracked<Float>

    init(base: Tracked<Float>) { self.base = base }

    @differentiable
    var squared: Tracked<Float> { base * base }

    @derivative(of: squared)
    final func vjpSquared() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> TangentVector) {
      let base = self.base
      return (base * base, { v in TangentVector(base: 2 * base * v) })
    }
  }

  class Sub1: Super {
    @differentiable
    override var squared: Tracked<Float> { base * base }
  }

  func classValueWithGradient(_ c: Super) -> (Tracked<Float>, Super.TangentVector) {
    return valueWithGradient(at: c) { c in c.squared }
  }

  expectEqual(4, gradient(at: 2) { x in Super(base: x).squared })
  expectEqual(Super.TangentVector(base: 4),
              gradient(at: Super(base: 2)) { foo in foo.squared })
}

ClassMethodTests.test("Capturing") {
  class Multiplier {
    var coefficient: Tracked<Float>
    init(_ coefficient: Tracked<Float>) {
      self.coefficient = coefficient
    }

    // Case 1: generated VJP.
    @differentiable
    func apply(to x: Tracked<Float>) -> Tracked<Float> {
      return coefficient * x
    }

    // Case 2: custom VJP capturing `self`.
    @differentiable(wrt: (x))
    func apply2(to x: Tracked<Float>) -> Tracked<Float> {
      return coefficient * x
    }
    @derivative(of: apply2)
    final func vjpApply2(
      to x: Tracked<Float>
    ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
      return (coefficient * x, { v in self.coefficient * v })
    }

    // Case 3: custom VJP capturing `self.coefficient`.
    @differentiable(wrt: x)
    func apply3(to x: Tracked<Float>) -> Tracked<Float> {
      return coefficient * x
    }
    @derivative(of: apply3)
    final func vjpApply3(
      to x: Tracked<Float>
    ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
      let coefficient = self.coefficient
      return (coefficient * x, { v in coefficient * v })
    }
  }

  func f(_ x: Tracked<Float>) -> Tracked<Float> {
    let m = Multiplier(10)
    let result = m.apply(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(10, gradient(at: 1, in: f))

  func f2(_ x: Tracked<Float>) -> Tracked<Float> {
    let m = Multiplier(10)
    let result = m.apply2(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(11, gradient(at: 1, in: f2))

  func f3(_ x: Tracked<Float>) -> Tracked<Float> {
    let m = Multiplier(10)
    let result = m.apply3(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(10, gradient(at: 1, in: f3))
}

runAllTests()
