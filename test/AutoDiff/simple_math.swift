// RUN: %target-swift-frontend -Xllvm -sil-print-after=differentiation %s -emit-sil -o /dev/null 2>&1 | %FileCheck %s
// RUN: %target-run-simple-swift
// NOTE(TF-813): verify that enabling forward-mode does not affect reverse-mode.
// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SimpleMathTests = TestSuite("SimpleMath")

SimpleMathTests.test("Arithmetics") {
  func foo1(x: Float, y: Float) -> Float {
    return x * y
  }
  expectEqual((4, 3), gradient(at: 3, 4, in: foo1))
  func foo2(x: Float, y: Float) -> Float {
    return -x * y
  }
  expectEqual((-4, -3), gradient(at: 3, 4, in: foo2))
  func foo3(x: Float, y: Float) -> Float {
    return -x + y
  }
  expectEqual((-1, 1), gradient(at: 3, 4, in: foo3))
}

SimpleMathTests.test("Fanout") {
  func foo1(x: Float) -> Float {
     x - x
  }
  expectEqual(0, gradient(at: 100, in: foo1))
  func foo2(x: Float) -> Float {
     x + x
  }
  expectEqual(2, gradient(at: 100, in: foo2))
  func foo3(x: Float, y: Float) -> Float {
    x + x + x * y
  }
  expectEqual((4, 3), gradient(at: 3, 2, in: foo3))
}

SimpleMathTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return 3 * x + { $0 * 3 }(3) * y
  }
  expectEqual((3, 9), gradient(at: 3, 4, in: foo))
  expectEqual(3, gradient(at: 3) { x in foo(x, 4) })
}

SimpleMathTests.test("ResultSelection") {
  func foo(_ x: Float, _ y: Float) -> (Float, Float) {
    return (x + 1, y + 2)
  }
  expectEqual((1, 0), gradient(at: 3, 3, in: { x, y in foo(x, y).0 }))
  expectEqual((0, 1), gradient(at: 3, 3, in: { x, y in foo(x, y).1 }))
}

SimpleMathTests.test("CaptureLocal") {
  let z: Float = 10
  func foo(_ x: Float) -> Float {
    return z * x
  }
  expectEqual(10, gradient(at: 0, in: foo))
}

var globalVar: Float = 10
SimpleMathTests.test("CaptureGlobal") {
  func foo(x: Float) -> Float {
    globalVar += 20
    return globalVar * x
  }
  expectEqual(30, gradient(at: 0, in: foo))
}

var foo_diffable: @differentiable (Float) -> (Float)
  = differentiableFunction { x in (x * x, { v in 2 * x * v }) }
SimpleMathTests.test("GlobalDiffableFunc") {
  expectEqual(2, gradient(at: 1, in: foo_diffable))
  expectEqual(2, gradient(at: 1, in: { x in foo_diffable(x) }))
  expectEqual(1, gradient(at: 1, in: { (x: Float) -> Float in
    foo_diffable = { x in x + 1 }
    return foo_diffable(x)
  }))
  expectEqual(1, gradient(at: 1, in: foo_diffable))
}

SimpleMathTests.test("SideEffects") {
  func fourthPower(x: Float) -> Float {
    var a = x
    a = a * x
    a = a * x
    return a * x
  }
  expectEqual(4 * 27, gradient(at: 3, in: fourthPower))
}

SimpleMathTests.test("Tuple") {
  // TF-945: Nested tuple projections.
  func nested(_ x: Float) -> Float {
    var tuple = (1, 1, ((x, 1), 1))
    return tuple.2.0.0
  }
  expectEqual(1, gradient(at: 3, in: nested))
}

SimpleMathTests.test("TupleSideEffects") {
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

  // FIXME(TF-201): Update after reabstraction thunks can be directly differentiated.
  /*
  func generic<T : Differentiable & AdditiveArithmetic>(_ x: T) -> T {
    var tuple = (x, x)
    tuple.0 += x
    tuple.1 += x
    return tuple.0 + tuple.0
  }
  expectEqual(1, gradient(at: 3.0, in: generic))
  */
}

// Tests TF-321.
SimpleMathTests.test("TupleNonDifferentiableElements") {
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
SimpleMathTests.test("StructMemberwiseInitializer") {
  struct Foo : AdditiveArithmetic, Differentiable {
    var stored: Float
    var computed: Float {
      return stored * stored
    }
  }

  let 𝛁foo = pullback(at: Float(4), in: { input -> Foo in
    let foo = Foo(stored: input)
    let foo2 = foo + foo
    return Foo(stored: foo2.stored)
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
  expectEqual(48, 𝛁product)

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
SimpleMathTests.test("StructConstantStoredProperty") {
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

SimpleMathTests.test("StructSideEffects") {
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
  expectEqual(Add.TangentVector(bias: 1),
              gradient(at: model) { m in m.applied(to: 1) })
}

SimpleMathTests.test("StructGeneric") {
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
  expectEqual(405, gradient(at: 3, in: fifthPower))
}

SimpleMathTests.test("StructWithNoDerivativeProperty") {
  struct NoDerivativeProperty : Differentiable {
    var x: Float
    @noDerivative var y: Float
  }
  expectEqual(
    NoDerivativeProperty.TangentVector(x: 1),
    gradient(at: NoDerivativeProperty(x: 1, y: 1)) { s -> Float in
      var tmp = s
      tmp.y = tmp.x
      return tmp.x
    }
  )
}

SimpleMathTests.test("SubsetIndices") {
  func grad(_ lossFunction: @differentiable (Float, Float) -> Float) -> Float {
    return gradient(at: 1) { x in lossFunction(x * x, 10.0) }
  }
  expectEqual(2, grad { x, y in x + y })

  func gradWRTNonDiff(_ lossFunction: @differentiable (Float, @nondiff Int) -> Float) -> Float {
    return gradient(at: 2) { x in lossFunction(x * x, 10) }
  }
  expectEqual(4, gradWRTNonDiff { x, y in x + Float(y) })
}

SimpleMathTests.test("ForceUnwrapping") {
  func forceUnwrap<T: Differentiable & FloatingPoint>(_ t: T)
    -> (T, Float) where T == T.TangentVector {
    gradient(at: t, Float(1)) { (x, y) in
      (x as! Float) * y
    }
  }
  expectEqual((1, 2), forceUnwrap(Float(2)))
}

// CHECK-LABEL: sil hidden [ossa] @AD__${{.*}}jumpTimesTwo{{.*}}pullback_src_0_wrt_0 : $@convention(thin) (Float, @owned _AD__$s4nullyycfU18_12jumpTimesTwoL_5modelSfAAyycfU18_14SmallTestModelL_V_tF_bb0__PB__src_0_wrt_0) -> SmallTestModel.TangentVector {
// CHECK: bb0([[DX:%.*]] : $Float,  [[PB_STRUCT:%.*]] : {{.*}}):
// CHECK-NEXT:   ([[PB0:%.*]], [[PB1:%.*]]) = destructure_struct [[PB_STRUCT]]
// CHECK-NEXT:   [[ADJ_TUPLE:%.*]] = apply [[PB1]]([[DX]]) : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK-NEXT:   destroy_value [[PB1]] : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK-NEXT:   ([[TMP0:%.*]], [[ADJ_CONCRETE:%.*]]) = destructure_tuple [[ADJ_TUPLE]] : $(Float, Float)
// CHECK-NEXT:   [[TMP1:%.*]] = apply [[PB0]]([[TMP0]]) : $@callee_guaranteed (Float) -> SmallTestModel.TangentVector
// CHECK-NEXT:   destroy_value [[PB0]] : $@callee_guaranteed (Float) -> SmallTestModel.TangentVector
// CHECK-NEXT:   [[ADJ_STRUCT_FIELD:%.*]] = destructure_struct [[TMP1]] : $SmallTestModel.TangentVector
// CHECK-NEXT:   [[TMP_RES:%.*]] = alloc_stack $Float
// CHECK-NEXT:   [[TMP_ADJ_STRUCT_FIELD:%.*]] = alloc_stack $Float
// CHECK-NEXT:   [[TMP_ADJ_CONCRETE:%.*]] = alloc_stack $Float
// CHECK-NEXT:   store [[ADJ_STRUCT_FIELD]] to [trivial] [[TMP_ADJ_STRUCT_FIELD]] : $*Float
// CHECK-NEXT:   store [[ADJ_CONCRETE]] to [trivial] [[TMP_ADJ_CONCRETE]] : $*Float
// CHECK-NEXT:   [[PLUS_EQUAL:%.*]] = witness_method $Float, #AdditiveArithmetic."+"!1 : <Self where Self : AdditiveArithmetic> (Self.Type) -> (Self, Self) -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK-NEXT:   [[METATYPE:%.*]] = metatype $@thick Float.Type
// CHECK-NEXT:   %{{.*}} = apply [[PLUS_EQUAL]]<Float>([[TMP_RES]], [[TMP_ADJ_CONCRETE]], [[TMP_ADJ_STRUCT_FIELD]], [[METATYPE]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK-NEXT:   destroy_addr [[TMP_ADJ_STRUCT_FIELD]] : $*Float
// CHECK-NEXT:   destroy_addr [[TMP_ADJ_CONCRETE]] : $*Float
// CHECK-NEXT:   dealloc_stack [[TMP_ADJ_CONCRETE]] : $*Float
// CHECK-NEXT:   dealloc_stack [[TMP_ADJ_STRUCT_FIELD]] : $*Float
// CHECK-NEXT:   [[RES:%.*]] = load [trivial] [[TMP_RES]] : $*Float
// CHECK-NEXT:   dealloc_stack [[TMP_RES]] : $*Float
// CHECK-NEXT:   [[RES_STRUCT:%.*]] = struct $SmallTestModel.TangentVector ([[RES]] : $Float)
// CHECK-NEXT:   return [[RES_STRUCT]] : $SmallTestModel.TangentVector
// CHECK-NEXT: }

SimpleMathTests.test("Struct") {
  // TF-943: Test adjoint value accumulation for aggregate lhs and concrete rhs.b
  struct SmallTestModel : Differentiable {
    public var jump: Float = 3.0
    @differentiable public func callAsFunction() -> Float { return jump }
  }

  func jumpTimesTwo(model: SmallTestModel) -> Float{
    // Add an aggregate and a concrete value.
    return model() + model.jump
  }
  let grads = gradient(at: SmallTestModel(), in: jumpTimesTwo)
  expectEqual(2.0, grads.jump)
}

runAllTests()
