// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardMode")

//===----------------------------------------------------------------------===//
// Basic tests.
//===----------------------------------------------------------------------===//

ForwardModeTests.test("Identity") {
  func func_to_diff(x: Float) -> Float {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4, in: func_to_diff)
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

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

ForwardModeTests.test("SubsetParametersDiff") {
  func func_to_diff1(x: Int, y: Float, z: Int) -> Float {
    return y
  }
  let (y1, differential1) = valueWithDifferential(at: 5) { y in
    func_to_diff1(x: 0, y: y, z: 0)
  }
  expectEqual(5, y1)
  expectEqual(1, differential1(1))

  func func_to_diff2(x: Float, y: Int, z: Int) -> Float {
    return 2 * x
  }
  let (y2, differential2) = valueWithDifferential(at: 6) { x in
    func_to_diff2(x: x, y: 0, z: 0) 
  }
  expectEqual(12, y2)
  expectEqual(2, differential2(1))

  func func_to_diff3(x: Int, y: Int, z: Float) -> Float {
    return 3 * z
  }
  let (y3, differential3) = valueWithDifferential(at: 7) { z in
    func_to_diff3(x: 0, y: 0, z: z) 
  }
  expectEqual(21, y3)
  expectEqual(3, differential3(1))
}

//===----------------------------------------------------------------------===//
// Functions with variables
//===----------------------------------------------------------------------===//

ForwardModeTests.test("UnaryWithVars") {
  func unary(x: Float) -> Float {
    var a = x
    a = x
    var b = a + 2
    b = b - 1
    let c: Float = 3
    var d = a + b + c - 1
    d = d + d
    return d
  }

  let (y, differential) = valueWithDifferential(at: 4, in: unary)
  expectEqual(22, y)
  expectEqual(4, differential(1))
}

//===----------------------------------------------------------------------===//
// Functions with basic struct
//===----------------------------------------------------------------------===//

struct A: Differentiable & AdditiveArithmetic {
  var x: Float
}

ForwardModeTests.test("StructInit") {
  func structInit(x: Float) -> A {
    return A(x: 2 * x)
  }

  let (y, differential) = valueWithDifferential(at: 4, in: structInit)
  expectEqual(A(x: 8), y)
  expectEqual(A(x: 2), differential(1))
}

ForwardModeTests.test("StructExtract") {
  func structExtract(x: A) -> Float {
    return 2 * x.x
  }

  let (y, differential) = valueWithDifferential(
    at: A(x: 4), 
    in: structExtract) 
  expectEqual(8, y)
  expectEqual(2, differential(A(x: 1)))
}

ForwardModeTests.test("LocalStructVariable") {
  func structExtract(x: A) -> A {
    let a = A(x: 2 * x.x) // 2x
    var b = A(x: a.x + 2) // 2x + 2
    b = A(x: b.x + a.x) // 2x + 2 + 2x = 4x + 2
    return b
  }

  let (y, differential) = valueWithDifferential(
    at: A(x: 4), 
    in: structExtract) 
  expectEqual(A(x: 18), y)
  expectEqual(A(x: 4), differential(A(x: 1)))
}

//===----------------------------------------------------------------------===//
// Functions with methods
//===----------------------------------------------------------------------===//

extension A {
  func noParamMethodA() -> A {
    return A(x: 2 * x)
  }

  func noParamMethodx() -> Float {
    return 2 * x
  }

  static func *(lhs: A, rhs: A) -> A {
    return A(x: lhs.x * rhs.x)
  }

  func complexBinaryMethod(u: A, v: Float) -> A {
    var b: A = u * A(x: 2)  // A(x: u * 2)
    b.x = b.x * v        // A(x: u * 2 * v)
    let c = b.x + 1      // u * 2 * v + 1

    // A(x: u * 2 * v + 1 + u * 2 * v) = A(x: x * (4uv + 1))
    return A(x: x * (c + b.x))
  }
}

ForwardModeTests.test("noParamMethodA") {
  let (y, differential) = valueWithDifferential(at: A(x: 4)) { x in
    x.noParamMethodA()
  }
  expectEqual(A(x: 8), y)
  expectEqual(A(x: 2), differential(A(x: 1)))
}

ForwardModeTests.test("noParamMethodx") {
  let (y, differential) = valueWithDifferential(at: A(x: 4)) { x in
    x.noParamMethodx()
  }
  expectEqual(8, y)
  expectEqual(2, differential(A(x: 1)))
}

ForwardModeTests.test("complexBinaryMethod") {
  let (y, differential) = valueWithDifferential(at: A(x: 4), A(x: 5), 3) { 
    (x, y, z) in
    // derivative = A(x: 4uv + 4xv + 4ux + 1) = 4*5*3 + 4*4*3 + 4*5*4 + 1 = 189
    x.complexBinaryMethod(u: y, v: z)
  }
  expectEqual(A(x: 244), y)
  expectEqual(A(x: 189), differential(A(x: 1), A(x: 1), 1))
}

//===----------------------------------------------------------------------===//
// Tracked struct
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

//===----------------------------------------------------------------------===//
// Tuples
//===----------------------------------------------------------------------===//

ForwardModeTests.test("SimpleTupleExtractLet") {
  func foo(_ x: Float) -> Float {
    let tuple = (2*x, x)
    return tuple.0
  }
  let (y, differential) = valueWithDifferential(at: 4, in: foo)
  expectEqual(8, y)
  expectEqual(2, differential(1))
}

ForwardModeTests.test("SimpleTupleExtractVar") {
  func foo(_ x: Float) -> Float {
    let tuple = (2*x, x)
    return tuple.0
  }
  let (y, differential) = valueWithDifferential(at: 4, in: foo)
  expectEqual(8, y)
  expectEqual(2, differential(1))
}

ForwardModeTests.test("TupleSideEffects") {
  func foo(_ x: Float) -> Float {
    var tuple = (x, x)
    tuple.0 = tuple.0 * x
    return x * tuple.0
  }
  expectEqual(27, derivative(at: 3, in: foo))

  func fifthPower(_ x: Float) -> Float {
    var tuple = (x, x)
    tuple.0 = tuple.0 * x
    tuple.1 = tuple.0 * x
    return tuple.0 * tuple.1
  }
  expectEqual(405, derivative(at: 3, in: fifthPower))

  func nested(_ x: Float) -> Float {
    var tuple = ((x, x), x)
    tuple.0.0 = tuple.0.0 * x
    tuple.0.1 = tuple.0.0 * x
    return tuple.0.0 * tuple.0.1
  }
  expectEqual(405, derivative(at: 3, in: nested))

  // FIXME(TF-201): Update after reabstraction thunks can be directly differentiated.
  /*
  func generic<T : Differentiable & AdditiveArithmetic>(_ x: T) -> T {
    var tuple = (x, x)
    tuple.0 += x
    tuple.1 += x
    return tuple.0 + tuple.0
  }
  expectEqual(1, derivative(at: 3.0, in: generic))
  */
}

// Tests TF-321.
ForwardModeTests.test("TupleNonDifferentiableElements") {
  // @differentiable
  func foo(_ x: Float) -> Float {
    var tuple = (x, 1)
    tuple.0 = x
    tuple.1 = 1
    return tuple.0
  }
  expectEqual(1, derivative(at: 1, in: foo))

  func bar(_ x: Float) -> Float {
    var tuple: (Int, Int, Float, Float) = (1, 1, x, x)
    tuple.0 = 1
    tuple.1 = 1
    tuple.3 = x
    return tuple.3
  }
  expectEqual(1, derivative(at: 1, in: bar))

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
  expectEqual(1, derivative(at: Float(1), in: { x -> Float in
    let wrapper = Wrapper<Float>()
    return wrapper.baz(x)
  }))
}

//===----------------------------------------------------------------------===//
// Generics
//===----------------------------------------------------------------------===//

struct Tensor<Scalar : FloatingPoint & Differentiable> 
  : VectorProtocol, Differentiable {
  // NOTE: `value` must have type with known size (e.g. `Float`, not `Scalar`)
  // until differentiation has indirect passing support.
  var value: Float
  init(_ value: Float) { self.value = value }
}

ForwardModeTests.test("GenericIdentity") {
  func identity<T : Differentiable>(_ x: T) -> T {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    identity(x) 
  }
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

ForwardModeTests.test("GenericTensorIdentity") {
  func identity<T : FloatingPoint & Differentiable>(
    _ x: Tensor<T>) -> Tensor<T> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    identity(Tensor<Float>(x)) 
  }
  expectEqual(Tensor<Float>(4), y)
  expectEqual(Tensor<Float>(1), differential(1))
}

ForwardModeTests.test("GenericTensorPlus") {
  func plus<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Float {
    return x.value + x.value
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in 
    plus(Tensor<Float>(x)) 
  }
  expectEqual(8, y)
  expectEqual(2, differential(1))
}

ForwardModeTests.test("GenericTensorBinaryInput") {
  func binary<T : FloatingPoint & Differentiable>(
    _ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    return x.value * y.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { 
    (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("GenericTensorWithLets") {
  func binary<T : FloatingPoint & Differentiable>(
    _ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    let a = Tensor<T>(x.value)
    let b = Tensor<T>(y.value)
    return a.value * b.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { 
    (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

ForwardModeTests.test("GenericTensorWithVars") {
  func binary<T : FloatingPoint & Differentiable>(
    _ x: Tensor<T>, _ y: Tensor<T>) -> Float {
    var a = Tensor<T>(x.value)
    var b = Tensor<T>(y.value)
    b = a
    a = Tensor<T>(y.value)
    return a.value * b.value
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { 
    (x: Float, y: Float) in 
    binary(Tensor<Float>(x), Tensor<Float>(y)) 
  }
  expectEqual(20, y)
  expectEqual(9, differential(1, 1))
}

// Test case where associated derivative function's requirements are met.
extension Tensor where Scalar : Numeric {
  @differentiable(wrt: self where Scalar : Differentiable & FloatingPoint)
  func mean() -> Tensor {
    return self
  }

  @differentiable(wrt: self where Scalar : Differentiable & FloatingPoint)
  func variance() -> Tensor {
    return mean() // ok
  }
}
_ = differential(at: Tensor<Float>(1), in: { $0.variance() })

// Tests TF-508: differentiation requirements with dependent member types.
protocol TF_508_Proto {
  associatedtype Scalar
}
extension TF_508_Proto where Scalar : FloatingPoint {
  @differentiable(
    jvp: jvpAdd
    where Self : Differentiable, Scalar : Differentiable,
          // Conformance requirement with dependent member type.
          Self.TangentVector : TF_508_Proto
  )
  static func +(lhs: Self, rhs: Self) -> Self {
    return lhs
  }

  @differentiable(
    jvp: jvpSubtract
    where Self : Differentiable, Scalar : Differentiable,
          // Same-type requirement with dependent member type.
          Self.TangentVector == Float
  )
  static func -(lhs: Self, rhs: Self) -> Self {
    return lhs
  }
}
extension TF_508_Proto where Self : Differentiable,
                             Scalar : FloatingPoint & Differentiable,
                             Self.TangentVector : TF_508_Proto {
  static func jvpAdd(lhs: Self, rhs: Self)
      -> (Self, (TangentVector, TangentVector) -> TangentVector) {
    return (lhs, { (dlhs, drhs) in dlhs })
  }
}
extension TF_508_Proto where Self : Differentiable,
                             Scalar : FloatingPoint & Differentiable,
                             Self.TangentVector == Float {
  static func jvpSubtract(lhs: Self, rhs: Self)
      -> (Self, (TangentVector, TangentVector) -> TangentVector) {
    return (lhs, { (dlhs, drhs) in dlhs })
  }
}

struct TF_508_Struct<Scalar : AdditiveArithmetic>
  : TF_508_Proto, AdditiveArithmetic {}
extension TF_508_Struct : Differentiable where Scalar : Differentiable {
  typealias TangentVector = TF_508_Struct
}

func TF_508() {
  let x = TF_508_Struct<Float>()
  // Test conformance requirement with dependent member type.
  _ = differential(at: x, in: { 
    (x: TF_508_Struct<Float>) -> TF_508_Struct<Float> in
    return x + x
  })
  // Test same-type requirement with dependent member type.
  _ = differential(at: x, in: { 
    (x: TF_508_Struct<Float>) -> TF_508_Struct<Float> in
    return x - x
  })
}

// TF-523
struct TF_523_Struct : Differentiable & AdditiveArithmetic {
  var a: Float = 1
  typealias TangentVector = TF_523_Struct
  typealias AllDifferentiableVariables = TF_523_Struct
}

@differentiable
func TF_523_f(_ x: TF_523_Struct) -> Float {
  return x.a * 2
}

// TF-534: Thunk substitution map remapping.
protocol TF_534_Layer : Differentiable {
  associatedtype Input : Differentiable
  associatedtype Output : Differentiable

  @differentiable
  func callAsFunction(_ input: Input) -> Output
}
struct TF_534_Tensor<Scalar> : Differentiable {}

func TF_534<Model: TF_534_Layer>(
  _ model: inout Model, inputs: Model.Input
) -> TF_534_Tensor<Float> where Model.Output == TF_534_Tensor<Float> {
  return valueWithDifferential(at: model) { model -> Model.Output in
    return model(inputs)
  }.0
}

// TODO: uncomment once control flow is supported in forward mode.
// TF-652: Test VJPEmitter substitution map generic signature.
// The substitution map should have the VJP's generic signature, not the
// original function's.
// struct TF_652<Scalar> {}
// extension TF_652 : Differentiable where Scalar : FloatingPoint {}

// @differentiable(wrt: x where Scalar: FloatingPoint)
// func test<Scalar: Numeric>(x: TF_652<Scalar>) -> TF_652<Scalar> {
//   for _ in 0..<10 {
//     let _ = x
//   }
//   return x
// }

// Tracked Generic.

ForwardModeTests.test("GenericTrackedIdentity") {
  func identity<T : Differentiable>(_ x: Tracked<T>) -> Tracked<T> {
    return x
  }
  let (y, differential) = valueWithDifferential(at: 4) { (x: Float) in
    identity(Tracked(x))
  }
  expectEqual(4, y)
  expectEqual(1, differential(1))
}

ForwardModeTests.test("GenericTrackedBinaryAdd") {
  func add<T>(_ x: Tracked<T>, _ y: Tracked<T>) -> Tracked<T>
    where T: Differentiable, T == T.TangentVector {
    return x + y
  }
  let (y, differential) = valueWithDifferential(at: 4, 5) { 
    (x: Float, y: Float) in
    add(Tracked(x), Tracked(y))
  }
  expectEqual(9, y)
  expectEqual(2, differential(1, 1))
}

ForwardModeTests.test("GenericTrackedBinaryLets") {
  func add<T>(_ x: Tracked<T>, _ y: Tracked<T>) -> Tracked<T>
    where T: Differentiable & SignedNumeric,
          T == T.TangentVector,
          T == T.Magnitude {
    let a = x * y // xy
    let b = a + a // 2xy
    return b + b // 4xy
  }
  // 4y + 4x
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in
    add(Tracked(x), Tracked(y))
  }
  expectEqual(80, y)
  expectEqual(36, differential(1, 1))
}

ForwardModeTests.test("GenericTrackedBinaryVars") {
  func add<T>(_ x: Tracked<T>, _ y: Tracked<T>) -> Tracked<T>
    where T: Differentiable & SignedNumeric,
          T == T.TangentVector,
          T == T.Magnitude {
    var a = x * y // xy
    a = a + a // 2xy
    var b = x
    b = a
    return b + b // 4xy
  }
  // 4y + 4x
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in
    add(Tracked(x), Tracked(y))
  }
  expectEqual(80, y)
  expectEqual(36, differential(1, 1))
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
  expectEqual(400, val1)
  expectEqual(160, dv1)
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
  expectEqual(50, y1)
  let c1 = Super.TangentVector(base: 1, _nontrivial: [])
  expectEqual(1, diff1(c1, 1))
  let (y2, diff2) = classDifferential(SubOverride(base: 5))
  expectEqual(30, y2)
  let c2 = SubOverride.TangentVector(base: 1, _nontrivial: [])
  expectEqual(3, diff2(c2, 1))
  let (y3, diff3) = classDifferential(SubOverrideCustomDerivatives(base: 5))
  expectEqual(30, y3)
  let c3 = SubOverrideCustomDerivatives.TangentVector(base: 1, _nontrivial: [])
  expectEqual(3, diff3(c3, 1))
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
  @differentiable(wrt: x)
  func foo(x: Float) -> Float
}

struct Linear: DiffReq, VectorProtocol {
  typealias TangentVector = Linear

  let m: Float
  let b: Float

  @differentiable(wrt: x)
  func foo(x: Float) -> Float {
    return m * x + b
  }
}

ForwardModeTests.test("Protocols") {
  func genericFoo<T: DiffReq>(_ t: T, _ x: Float) -> Float {
    t.foo(x: x)
  }
  let inst = Linear(m: 5, b: -2)
  let (y1, diff1) = valueWithDifferential(at: 5) { x in genericFoo(inst, x) }
  expectEqual(23, y1)
  expectEqual(5, diff1(1))
}

runAllTests()
