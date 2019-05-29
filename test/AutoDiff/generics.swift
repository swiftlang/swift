// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s -check-prefix=CHECK-SIL

@_silgen_name("identity")
func identity<T : Differentiable>(_ x: T) -> T {
  return x
}
_ = gradient(at: Float(1), in: { x in identity(x) })

// Test AdjointEmitter local buffer allocation.
// Verify that local buffers are immediately set to zero.

// CHECK-SIL-LABEL: sil hidden @AD__identity__adjoint_src_0_wrt_0
// CHECK-SIL:      [[ORIG_COTAN:%.*]] = alloc_stack $τ_0_0.TangentVector
// CHECK-SIL-NEXT: [[ORIG_COTAN_BEGIN:%.*]] = begin_access [init] [static] [no_nested_conflict] [[ORIG_COTAN]]
// CHECK-SIL-NEXT: [[ZERO_WITNESS:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter.1
// CHECK-SIL-NEXT: [[ORIG_COTAN_METATYPE:%.*]] = metatype $@thick τ_0_0.TangentVector.Type
// CHECK-SIL-NEXT: [[EMIT_ZERO_INDIRECT:%.*]] = apply [[ZERO_WITNESS]]<τ_0_0.TangentVector>([[ORIG_COTAN_BEGIN]], [[ORIG_COTAN_METATYPE]])
// CHECK-SIL-NEXT: end_access [[ORIG_COTAN_BEGIN]]
// CHECK-SIL: }

struct Tensor<Scalar : FloatingPoint & Differentiable> : VectorNumeric, Differentiable {
  // NOTE: `value` must have type with known size (e.g. `Float`, not `Scalar`)
  // until differentiation has indirect passing support.
  var value: Float
  init(_ value: Float) { self.value = value }
}

func generic<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Float {
  return x.value + x.value
}
_ = gradient(at: Tensor<Float>(1), in: generic)

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
_ = pullback(at: Tensor<Float>(1), in: { $0.variance() })

// Tests TF-277.
protocol Layer : Differentiable {
  associatedtype Output : Differentiable
}
struct SupervisedTrainer<Model : Layer> {
  var model: Model
  var lossFunction: @differentiable (Model.Output, Model.Output) -> Float
  func fit(y: Model.Output) {
    _ = gradient(at: y) { y in return lossFunction(y, y) }
  }
}

// Tests TF-440.
struct TF_440_Input<Input: Differentiable, State: Differentiable>
  : Differentiable {
  var input: Input
  var state: State
}
struct TF_440<T : Differentiable> {
  @differentiable
  func applied(to input: TF_440_Input<Float, Float>) -> Float {
    return input.state
  }

  @differentiable
  func applied(to input: TF_440_Input<T, Float>) -> Float {
    return input.state
  }

  @differentiable
  func applied(to input: TF_440_Input<T, Float>) -> T {
    return input.input
  }
}

// Tests TF-508
protocol TF_508_Protocol {
  associatedtype Scalar: BinaryFloatingPoint
}

extension TF_508_Protocol {
  @differentiable(vjp: _vjpAdd(lhs:rhs:)
    where Self : Differentiable,
          Scalar : Differentiable,
          TangentVector : TF_508_Protocol)
  static func +(lhs: Self, rhs: Self) -> Self {
    return lhs
  }
  
  static var zero: Self {
    fatalError()
  }
  
  static func - (lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
}

extension TF_508_Protocol
  where Self : Differentiable,
        Scalar : Differentiable,
        TangentVector : TF_508_Protocol {
  static func _vjpAdd(lhs: Self, rhs: Self)
    -> (Self, (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs, { ($0, $0) })
  }
}

struct TF_508_Struct<Scalar: BinaryFloatingPoint>
  : TF_508_Protocol & AdditiveArithmetic {}

extension TF_508_Struct : Differentiable
  where Scalar : Differentiable {
  typealias TangentVector = TF_508_Struct
  typealias AllDifferentiableVariables = TF_508_Struct
}

let TF_508_inst = TF_508_Struct<Float>()
func TF_508_func(x: TF_508_Struct<Float>, y: TF_508_Struct<Float>)
  -> TF_508_Struct<Float> {
  return x + y
}
let TF_508_bp = pullback(at: TF_508_inst, TF_508_inst, in: TF_508_func)

// TODO: add more tests.
