//===-- Gradients.swift ---------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains gradient definitions for Tensor ops.
//
// Terminology:
// - partial (f): The function being differentiated, or the result of that
//   function.
// - Adjoint (f'): The function as the result of differentiation, computing
//   the Jacobian-vector products or gradients with respect to all arguments,
//   or the result of that function.
// - Seed: The back-propagated adjoint, i.e. the adjoint of the caller of the
//   function with respect to the result of the function.
//
// For more information, visit:
// https://en.wikipedia.org/wiki/Automatic_differentiation
//
// Each function in this file is the adjoint of some corresponding function
// defined in Ops.swift with respect to all of its parameters. The attribute
// '@differentiable(gradient: ...)' is used to define the adjoint for a partial
// function. The automatic differentiation pass will pick up these adjoints
// and chain them together for arbitrary differentiable programs.
//
// NOTE:
// - Currently, we do not want to expose adjoint functions to users. The name of
//   each adjoint function should start with an underscore.
// TODO:
// - Add gradients for more ops ('sum', 'mean', etc).
// - Fix gradients for broadcasting ops (need to perform reduction).
// FIXME:
// - Handle scalar broadcasting.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Elementwise binary
//===----------------------------------------------------------------------===//

extension TensorProtocol where Scalar : Numeric {
  @_inlineable @_versioned
  static func _adjointAdd(
    _: Self, _: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed, seed)
  }

  @_inlineable @_versioned
  static func _adjointSubtract(
    _: Self, _: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed, -seed)
  }

  @_inlineable @_versioned
  static func _adjointMultiply(
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (y * seed, x * seed)
  }

  @_inlineable @_versioned
  static func _adjointDivide(
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed / y, -x / y.squared() * seed)
  }
}

@_inlineable @_versioned
func _adjointMin<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: T, partial: T, seed: T
) -> (T, T) where T.Scalar == Scalar {
  let denom = 1 + T(lhs == rhs)
  let dfdx = seed * T(rhs == partial) / denom
  let dfdy = seed * T(lhs == partial) / denom
  return (dfdx, dfdy)
}

@_inlineable @_versioned
func _adjointMax<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: T, partial: T, seed: T
) -> (T, T) where T.Scalar == Scalar {
  let denom = 1 + T(lhs == rhs)
  let dfdx = seed * T(lhs == partial) / denom
  let dfdy = seed * T(rhs == partial) / denom
  return (dfdx, dfdy)
}

@_inlineable @_versioned
func _adjointPow<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, _ y: T, partial: T, seed: T
) -> (T, T) where T.Scalar == Scalar {
  return (seed * y * pow(x, y-1), seed * log(x) * partial)
}

//===----------------------------------------------------------------------===//
// Elementwise unary
//===----------------------------------------------------------------------===//

extension TensorProtocol where Scalar : Numeric {
  @_inlineable @_versioned
  static func _adjointNegate(_ x: Self, partial: Self, seed: Self) -> Self {
    return -seed
  }
}

@_inlineable @_versioned
func _adjointLog<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed / x
}

@_inlineable @_versioned
func _adjointSin<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed * cos(x)
}

@_inlineable @_versioned
func _adjointCos<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return -seed * cos(x)
}

@_inlineable @_versioned
func _adjointTan<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed / (1 + partial.squared())
}

@_inlineable @_versioned
func _adjointSinh<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed * cosh(x)
}

@_inlineable @_versioned
func _adjointCosh<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed * sinh(x)
}

@_inlineable @_versioned
func _adjointTanh<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed * (1 - partial.squared())
}

@_inlineable @_versioned
func _adjointExp<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed
}

@_inlineable @_versioned
func _adjointSqrt<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return seed / (2 * partial)
}

@_inlineable @_versioned
func _adjointSquared<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return 2 * x * seed
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

extension TensorProtocol where Scalar : Numeric {
  @_inlineable @_versioned
  func _adjointDot(
    _ other: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed.dot(other.transposed()), transposed().dot(seed))
  }
}

extension TensorProtocol {
  @_inlineable @_versioned
  func _adjointTransposed(
    _ permutations: Tensor<Int32>, partial: Self, seed: Self
  ) -> Self {
    return seed.transposed(withPermutations: permutations)
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

extension Tensor {
  @_inlineable @_versioned
  func _adjointReshaped(
    toShape newShape: Tensor<Int32>, partial: Tensor, seed: Tensor
  ) -> Tensor {
    return seed.reshaped(toShape: shapeTensor)
  }

  @_inlineable @_versioned
  func _adjointExpandingShape(
    at shapeIndex: Int32, partial: Tensor, seed: Tensor
  ) -> Tensor {
    return seed.squeezingShape(at: shapeIndex)
  }

  @_inlineable @_versioned
  func _adjointSqueezingShape(
    at axes: Int32, partial: Tensor, seed: Tensor
  ) -> Tensor {
    // TODO: need to formulate variadic ExpandDims using tensor code.
    fatalError("Unimplemented")
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : FloatingPoint {
  /// TensorFlow builtin conv2d gradient helper for the input.
  @_inlineable @_versioned
  @differentiable(
    withRespectTo: (.1, .2),
    gradient: _adjointTFConv2DBackpropInput(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropInput(
    shape: Tensor<Int32>,
    filter: Tensor,
    backpropOutput: Tensor,
    strides: [Int32],
    padding: Padding
  ) -> Tensor {
    return #tfop("Conv2DBackpropInput", shape, filter, backpropOutput,
      strides: strides, padding: padding)
  }

  /// TensorFlow builtin conv2d gradient helper for the filter.
  @_inlineable @_versioned
  @differentiable(
    withRespectTo: (.0, .2),
    gradient: _adjointTFConv2DBackpropFilter(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropFilter(
    input: Tensor,
    filterSizes: Tensor<Int32>,
    backpropOutput: Tensor,
    strides: [Int32],
    padding: Padding
  ) -> Tensor {
    return #tfop("Conv2DBackpropFilter", input, filterSizes, backpropOutput,
      strides: strides, padding: padding.cName)
  }

  @_inlineable @_versioned
  func _adjointTFConv2DBackpropInput(
    _ shape: Tensor<Int32>,
    _ filter: Tensor,
    _ backpropOutput: Tensor,
    _ strides: [Int32],
    _ padding: Padding,
    _ partial: Tensor,
    _ seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropFilter(input: seed, filterSizes: shape,
        backpropOutput: backpropOutput, strides: strides,
        padding: padding),
      seed.convolved2D(withFilter: filter, strides: strides, padding: padding)
    )
  }

  @_inlineable @_versioned
  func _adjointTFConv2DBackpropFilter(
    _ input: Tensor,
    _ filterSizes: Tensor<Int32>,
    _ backpropOutput: Tensor,
    _ strides: [Int32],
    _ padding: Padding,
    _ partial: Tensor,
    _ seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropInput(shape: filterSizes, filter: seed,
        backpropOutput: backpropOutput, strides: strides,
        padding: padding),
      input.convolved2D(withFilter: seed, strides: strides, padding: padding)
    )
  }

  @_inlineable @_versioned
  func _adjointConvolved2D(
    filter: Tensor,
    strides: [Int32],
    padding: Padding,
    partial: Tensor,
    seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropInput(shape: shapeTensor, filter: filter,
        backpropOutput: seed, strides: strides,
        padding: padding),
      _TFConv2DBackpropFilter(input: self, filterSizes: filter.shapeTensor,
        backpropOutput: seed, strides: strides,
        padding: padding
      )
    )
  }

  @_inlineable @_versioned
  func _adjointMaxPooled(
    kernelSize: Tensor<Int32>,
    strides: Tensor<Int32>,
    padding: Padding,
    partial: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return #tfop("MaxPoolGradV2", shapeTensor, partial, seed, kernelSize,
                 strides, padding: padding.cName)
  }

  @_inlineable @_versioned
  func _adjointAveragePooled(
    kernelSize: [Int32],
    strides: [Int32],
    padding: Padding,
    partial: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return #tfop("AvgPoolGrad", shapeTensor, seed, ksize: kernelSize,
                 strides: strides, padding: padding.cName)
  }
}

//===----------------------------------------------------------------------===//
// Composite math
//===----------------------------------------------------------------------===//

@_inlineable @_versioned
func _adjointRelu<Scalar : FloatingPoint, T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar == Scalar {
  return T(x > 0)
}
