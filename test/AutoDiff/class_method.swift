// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var ClassMethodTests = TestSuite("ClassMethods")

ClassMethodTests.test("Final") {
  final class Final : Differentiable {
    func method(_ x: Float) -> Float {
      return x * x
    }
  }

  for i in -5...5 {
    expectEqual(Float(i) * 2, gradient(at: Float(i)) { x in Final().method(x) })
  }
}

ClassMethodTests.test("Simple") {
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

  func classValueWithGradient(_ c: Super) -> (Float, Float) {
    return valueWithGradient(at: 1) { c.f($0) }
  }
  expectEqual((2, 2), classValueWithGradient(Super()))
  expectEqual((3, 3), classValueWithGradient(SubOverride()))
  expectEqual((3, 3), classValueWithGradient(SubOverrideCustomDerivatives()))
}

ClassMethodTests.test("SimpleWrtSelf") {
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
  /*
  let v = Super.TangentVector(base: 100, _nontrivial: [])
  expectEqual(100, pullback(at: 1337) { x in Super(base: x) }(v))
  expectEqual(100, pullback(at: 1337) { x in SubOverride(base: x) }(v))
  expectEqual(100, pullback(at: 1337) { x in SubOverrideCustomDerivatives(base: x) }(v))
  */

  // `valueWithGradient` is not used because nested tuples cannot be compared
  // with `expectEqual`.
  func classGradient(_ c: Super) -> (Super.TangentVector, Float) {
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
  class Super<T : Differentiable & FloatingPoint> where T == T.TangentVector {
    @differentiable(wrt: x, jvp: jvpf, vjp: vjpf)
    func f(_ x: T) -> T {
      return T(2) * x
    }
    final func jvpf(_ x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
      return (f(x), { v in T(2) * v })
    }
    final func vjpf(_ x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
      return (f(x), { v in T(2) * v })
    }
  }

  class SubOverride<T : Differentiable & FloatingPoint> : Super<T> where T == T.TangentVector {
    @differentiable(wrt: x)
    override func f(_ x: T) -> T {
      return x
    }
  }

  class SubSpecializeOverride : Super<Float> {
    @differentiable(wrt: x)
    override func f(_ x: Float) -> Float {
      return 3 * x
    }
  }

  class SubOverrideCustomDerivatives<T : Differentiable & FloatingPoint> : Super<T> where T == T.TangentVector {
    @differentiable(wrt: x, jvp: jvpf2, vjp: vjpf2)
    override func f(_ x: T) -> T {
      return T(3) * x
    }
    final func jvpf2(_ x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
      return (f(x), { v in T(3) * v })
    }
    final func vjpf2(_ x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
      return (f(x), { v in T(3) * v })
    }
  }

  class SubSpecializeOverrideCustomDerivatives : Super<Float80> {
    @differentiable(wrt: x, jvp: jvpf2, vjp: vjpf2)
    override func f(_ x: Float80) -> Float80 {
      return 3 * x
    }
    final func jvpf2(_ x: Float80) -> (Float80, (Float80) -> Float80) {
      return (f(x), { v in 3 * v })
    }
    final func vjpf2(_ x: Float80) -> (Float80, (Float80) -> Float80) {
      return (f(x), { v in 3 * v })
    }
  }

  func classValueWithGradient<T : Differentiable & FloatingPoint>(
    _ c: Super<T>
  ) -> (T, T) where T == T.TangentVector {
    return valueWithGradient(at: T(1)) { c.f($0) }
  }
  expectEqual((2, 2), classValueWithGradient(Super<Float>()))
  expectEqual((1, 1), classValueWithGradient(SubOverride<Float>()))
  expectEqual((3, 3), classValueWithGradient(SubSpecializeOverride()))
  expectEqual((3, 3), classValueWithGradient(SubOverrideCustomDerivatives<Float>()))
  expectEqual((3, 3), classValueWithGradient(SubSpecializeOverrideCustomDerivatives()))
}

ClassMethodTests.test("Methods") {
  class Super : Differentiable {
    var base: Float
    // Dummy to make `Super.AllDifferentiableVariables` be nontrivial.
    var _nontrivial: [Float] = []

    // TODO(TF-654): Uncomment attribute when differentiation supports class initializers.
    // TODO(TF-645): Remove `vjpInit` when differentiation supports `ref_element_addr`.
    // @differentiable(vjp: vjpInit)
    init(base: Float) {
      self.base = base
    }
    static func vjpInit(base: Float) -> (Super, (TangentVector) -> Float) {
      return (Super(base: base), { x in x.base })
    }

    @differentiable(vjp: vjpSquared)
    func squared() -> Float { base * base }

    final func vjpSquared() -> (Float, (Float) -> TangentVector) {
      let base = self.base
      return (base * base, { v in
        TangentVector(base: 2 * base * v, _nontrivial: [])
      })
    }
  }

  class Sub1 : Super {
    @differentiable(vjp: vjpSquared2)
    override func squared() -> Float { base * base }
    final func vjpSquared2() -> (Float, (Float) -> TangentVector) {
      let base = self.base
      return (base * base, { v in
        TangentVector(base: 2 * base * v, _nontrivial: [])
      })
    }
  }

  func classValueWithGradient(_ c: Super) -> (Float, Super.TangentVector) {
    return valueWithGradient(at: c) { c in c.squared() }
  }

  // TODO(TF-654, TF-645): Uncomment when differentiation supports class initializers or `ref_element_addr`.
  // expectEqual(4, gradient(at: 2) { x in Super(base: x).squared() })

  // TODO(TF-647): Handle `unchecked_ref_cast` in `Sub1.init` during pullback generation.
  // FIXME: `Super.init` VJP type mismatch for empty `Super.AllDifferentiableVariables`:
  // SIL verification failed: VJP type does not match expected VJP type
  //   $@convention(method) (Float, @thick Super.Type) -> (@owned Super, @owned @callee_guaranteed (@guaranteed Super.AllDifferentiableVariables) -> Float)
  //   $@convention(method) (Float, @owned Super) -> (@owned Super, @owned @callee_guaranteed (@guaranteed Super.AllDifferentiableVariables) -> Float)
  // expectEqual(4, gradient(at: 2) { x in Sub1(base: x).squared() })

  expectEqual(Super.TangentVector(base: 4, _nontrivial: []),
              gradient(at: Super(base: 2)) { foo in foo.squared() })
  expectEqual(Sub1.TangentVector(base: 4, _nontrivial: []),
              gradient(at: Sub1(base: 2)) { foo in foo.squared() })
}

ClassMethodTests.test("Properties") {
  class Super : Differentiable {
    var base: Float

    // TODO(TF-654): Uncomment attribute when differentiation supports class initializers.
    // TODO(TF-645): Remove `vjpInit` when differentiation supports `ref_element_addr`.
    // @differentiable(vjp: vjpInit)
    init(base: Float) { self.base = base }
    static func vjpInit(base: Float) -> (Super, (TangentVector) -> Float) {
      return (Super(base: base), { x in x.base })
    }

    @differentiable(vjp: vjpSquared)
    var squared: Float { base * base }

    final func vjpSquared() -> (Float, (Float) -> TangentVector) {
      let base = self.base
      return (base * base, { v in TangentVector(base: 2 * base * v) })
    }
  }

  class Sub1 : Super {
    // FIXME(TF-625): Crash due to `Super.AllDifferentiableVariables` abstraction pattern mismatch.
    // SIL verification failed: vtable entry for #<anonymous function>Super.squared!getter.1.jvp.1.S must be ABI-compatible
    //   ABI incompatible return values
    //   @convention(method) (@guaranteed Super) -> (Float, @owned @callee_guaranteed (@guaranteed Super.AllDifferentiableVariables) -> Float)
    //   @convention(method) (@guaranteed Sub1) -> (Float, @owned @callee_guaranteed (Super.AllDifferentiableVariables) -> Float)
    // @differentiable
    // override var squared: Float { base * base }
  }

  func classValueWithGradient(_ c: Super) -> (Float, Super.TangentVector) {
    return valueWithGradient(at: c) { c in c.squared }
  }

  // TODO(TF-654, TF-645): Uncomment when differentiation supports class initializers or `ref_element_addr`.
  // expectEqual(4, gradient(at: 2) { x in Super(base: x).squared })
  expectEqual(Super.TangentVector(base: 4),
              gradient(at: Super(base: 2)) { foo in foo.squared })
}

ClassMethodTests.test("Capturing") {
  class Multiplier {
    var coefficient: Float
    init(_ coefficient: Float) {
      self.coefficient = coefficient
    }

    // Case 1: generated VJP.
    @differentiable
    func apply(to x: Float) -> Float {
      return coefficient * x
    }

    // Case 2: custom VJP capturing `self`.
    @differentiable(wrt: (x), vjp: vjpApply2)
    func apply2(to x: Float) -> Float {
      return coefficient * x
    }
    final func vjpApply2(to x: Float) -> (Float, (Float) -> Float) {
      return (coefficient * x, { v in self.coefficient * v })
    }

    // Case 3: custom VJP capturing `self.coefficient`.
    @differentiable(wrt: x, vjp: vjpApply3)
    func apply3(to x: Float) -> Float {
      return coefficient * x
    }
    final func vjpApply3(to x: Float) -> (Float, (Float) -> Float) {
      let coefficient = self.coefficient
      return (coefficient * x, { v in coefficient * v })
    }
  }

  func f(_ x: Float) -> Float {
    let m = Multiplier(10)
    let result = m.apply(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(10, gradient(at: 1, in: f))

  func f2(_ x: Float) -> Float {
    let m = Multiplier(10)
    let result = m.apply2(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(11, gradient(at: 1, in: f2))

  func f3(_ x: Float) -> Float {
    let m = Multiplier(10)
    let result = m.apply3(to: x)
    m.coefficient += 1
    return result
  }
  expectEqual(10, gradient(at: 1, in: f3))
}

runAllTests()
