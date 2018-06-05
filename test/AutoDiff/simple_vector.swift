// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

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

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin.C
#else
import Glibc
#endif

@_silgen_name("sin")
@differentiable(reverse, adjoint: sin_adj)
func sin(_ a: Vector) -> Vector {
  return Vector(x: sin(a.x), y: sin(a.y))
}

@_silgen_name("sin_adj")
func sin_adj(_ a: Vector, originalValue: Vector, seed: Vector) -> Vector {
  return Vector(x: cos(a.x), y: cos(a.y))
}

@_silgen_name("just_gradient_of_sin")
public func just_gradient_of_sin() {
  let vec = Vector(10)
  let dsin = #gradient(of: sin)
  _ = dsin(vec)
}

// CHECK-LABEL: sil hidden @sin__grad_src_0_wrt_0

// CHECK-LABEL: sil @just_gradient_of_sin : $convention(thin) () -> ()
// CHECK: bb0:
// CHECK: [[SIN:%.*]] = function_ref @sin
// CHECK: [[ORIGVAL:%.*]] = apply [[SIN]]([[X:%.*]])
// CHECK: [[SINADJ:%.*]] = function_ref @sin_adj
// CHECK: apply [[SINADJ]]
