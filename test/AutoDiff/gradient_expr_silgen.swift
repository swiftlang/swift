// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

struct Vector {
  let x: Float
  let y: Float
}

extension Vector : VectorNumeric {
  typealias ScalarElement = Float
  typealias Dimensionality = Int
  init(_ scalar: ScalarElement) {
    self.init(x: scalar, y: scalar)
  }
  init(dimensionality: Int, repeating value: ScalarElement) {
    precondition(dimensionality == 2)
    self.init(value)
  }
  static func + (a: Vector, b: Vector) -> Vector {
    return Vector(x: a.x+b.x, y: a.y+b.y)
  }
  static func - (a: Vector, b: Vector) -> Vector {
    return Vector(x: a.x-b.x, y: a.y-b.y)
  }
  static func * (a: Vector, b: Vector) -> Vector {
    return Vector(x: a.x*b.x, y: a.y*b.y)
  }
}

@_silgen_name("foo")
public func foo(_ x: Float, _ y: Float) -> Float {
  return x * y
}

@_silgen_name("foo2")
public func foo2(_ x: Float, _ y: Float) -> (Float, Float) {
  return (x, y)
}

@_silgen_name("foo_indir_ret")
public func foo_indir_ret<T>(x: Float, y: Float, t: T) -> (T, T) {
  return (t, t)
}

@_silgen_name("foo_generic_vector")
public func foo_generic_vector<T : VectorNumeric>(x: T, y: T) -> T where T.ScalarElement : FloatingPoint {
  return x
}

@_silgen_name("foo_generic_vector_and_scalar")
public func foo_generic_vector_and_scalar<T : FloatingPoint, U : VectorNumeric>(x: T, y: U) -> U where U.ScalarElement : FloatingPoint { return y }

let _ = #gradient(foo)
let _ = #gradient(foo, wrt: .0)
let _ = #valueAndGradient(foo)
let _ = #gradient(foo2, result: .1)
let _: (Float, Float, [Int]) -> (Float, Float) = #gradient(foo_indir_ret, wrt: .0, .1)
let _: (Float, Float, [Int]) -> (Float, Float) = #gradient(foo_indir_ret, result: .1, wrt: .0, .1)
let _: (Vector, Vector) -> (Vector, Vector) = #gradient(foo_generic_vector)
let _: (Float, Vector) -> (Float, Vector) = #gradient(foo_generic_vector_and_scalar)

// CHECK-LABEL: sil @main :
// CHECK: [[FOO_1:%.*]] = function_ref @foo
// CHECK: [[FOO_1_THICK:%.*]] = thin_to_thick_function [[FOO_1]]
// CHECK: gradient [source 0] [wrt 0, 1] [[FOO_1_THICK]] : $@callee_guaranteed (Float, Float) -> Float

// CHECK: [[FOO_2:%.*]] = function_ref @foo
// CHECK: [[FOO_2_THICK:%.*]] = thin_to_thick_function [[FOO_2]]
// CHECK: gradient [source 0] [wrt 0] [[FOO_2_THICK]] : $@callee_guaranteed (Float, Float) -> Float

// CHECK: [[FOO_3:%.*]] = function_ref @foo
// CHECK: [[FOO_3_THICK:%.*]] = thin_to_thick_function [[FOO_3]]
// CHECK: gradient [source 0] [wrt 0, 1] [preserving_result] [[FOO_3_THICK]] : $@callee_guaranteed (Float, Float) -> Float

// CHECK: [[FOO2:%.*]] = function_ref @foo2
// CHECK: [[FOO2_THICK:%.*]] = thin_to_thick_function [[FOO2]]
// CHECK: gradient [source 1] [wrt 0, 1] [[FOO2_THICK]] : $@callee_guaranteed (Float, Float) -> (Float, Float)

// CHECK: [[FOO_INDIR_RET:%.*]] = function_ref @foo_indir_ret
// CHECK: gradient [source 0] [wrt 0, 1] {{%.*}} : $@callee_guaranteed (Float, Float, @guaranteed Array<Int>) -> (@owned Array<Int>, @owned Array<Int>)

// CHECK: [[FOO_INDIR_RET:%.*]] = function_ref @foo_indir_ret
// CHECK: gradient [source 1] [wrt 0, 1] {{%.*}} : $@callee_guaranteed (Float, Float, @guaranteed Array<Int>) -> (@owned Array<Int>, @owned Array<Int>)

// CHECK: [[FOO_GENERIC_VECTOR:%.*]] = function_ref @foo_generic_vector
// CHECK: gradient [source 0] [wrt 0, 1] {{%.*}} : $@callee_guaranteed (Vector, Vector) -> Vector

// CHECK: [[FOO_GENERIC_VECTOR_AND_SCALAR:%.*]] = function_ref @foo_generic_vector_and_scalar
// CHECK: gradient [source 0] [wrt 0, 1] {{%.*}} : $@callee_guaranteed (Float, Vector) -> Vector
