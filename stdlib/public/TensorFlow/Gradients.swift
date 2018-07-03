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
// - originalValue (f): The function being differentiated, or the result of that
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
// '@differentiable(reverse, adjoint: ...)' is used to define the adjoint for a
// function. The automatic differentiation pass will pick up these adjoints
// and chain them together for arbitrary differentiable programs.
//
// NOTE:
// - Currently, we do not want to expose adjoint functions to users. The name of
//   each adjoint function should start with an underscore.
// TODO:
// - Add gradients for more ops ('sum', 'mean', etc).
// - Fix gradients for broadcasting ops (need to perform reduction).
// - When the trailing 'where' clause in @differentiable is properly
//   type-checked, define constraints on BinaryFloatingPoint in original
//   declarations and define adjoints on BinaryFloatingPoint.
//
// FIXME:
// - Handle scalar broadcasting.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Elementwise binary
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : Numeric {
  @inlinable
  static func _adjointAdd(
    _ x: Tensor, _ y: Tensor, originalValue: Tensor, seed: Tensor
  ) -> (Tensor, Tensor) {
    let seed = seed.broadcast(like: originalValue)
    return (seed.unbroadcast(like: x), seed.unbroadcast(like: y))
  }

  @inlinable
  static func _adjointSubtract(
    _ x: Tensor, _ y: Tensor, originalValue: Tensor, seed: Tensor
  ) -> (Tensor, Tensor) {
    let seed = seed.broadcast(like: originalValue)
    return (seed.unbroadcast(like: x), 0 - seed.unbroadcast(like: y))
  }

  @inlinable
  static func _adjointMultiply(
    _ x: Tensor, _ y: Tensor, originalValue: Tensor, seed: Tensor
  ) -> (Tensor, Tensor) {
    return ((y * seed).unbroadcast(like: x),
            (x * seed).unbroadcast(like: y))
  }

  @inlinable
  static func _adjointDivide(
    _ x: Tensor, _ y: Tensor, originalValue: Tensor, seed: Tensor
  ) -> (Tensor, Tensor) {
    return ((seed / y).unbroadcast(like: x),
            ((0 - x) / y.squared() * seed).unbroadcast(like: y))
  }
}

@inlinable
func _adjointMinMax<T : Numeric & Comparable>(
  _ x: Tensor<T>, _ y: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  let denom = 1 + Tensor<T>(x.elementsEqual(y))
  let dfdx = seed * Tensor<T>(x.elementsEqual(originalValue)) / denom
  let dfdy = seed * Tensor<T>(y.elementsEqual(originalValue)) / denom
  return (dfdx.unbroadcast(like: x), dfdy.unbroadcast(like: y))
}

@inlinable
func _adjointPow<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return ((seed * y * pow(x, y-1)).unbroadcast(like: x),
          (seed * log(x) * originalValue).unbroadcast(like: y))
}

//===----------------------------------------------------------------------===//
// Elementwise unary
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : SignedNumeric {
  @inlinable
  static func _adjointNegate(
    _ x: Tensor, originalValue: Tensor, seed: Tensor
  ) -> Tensor {
    return -seed.broadcast(like: originalValue)
  }
}

@inlinable
func _adjointLog<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed / x
}

@inlinable
func _adjointSin<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * cos(x)
}

@inlinable
func _adjointCos<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return -seed * sin(x)
}

@inlinable
func _adjointTan<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * (1 + originalValue.squared())
}

@inlinable
func _adjointSinh<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * cosh(x)
}

@inlinable
func _adjointCosh<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * sinh(x)
}

@inlinable
func _adjointTanh<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * (1 - originalValue.squared())
}

@inlinable
func _adjointExp<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return originalValue * seed
}

@inlinable
func _adjointSqrt<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed / (2 * originalValue)
}

@inlinable
func _adjointRsqrt<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return -seed / 2 * pow(originalValue, 3)
}

func _adjointSquared<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return 2 * x * seed
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

@inlinable
func _adjointMatmul<Scalar : Numeric>(
  _ left: Tensor<Scalar>, _ right: Tensor<Scalar>,
  originalValue: Tensor<Scalar>, seed: Tensor<Scalar>
) -> (Tensor<Scalar>, Tensor<Scalar>) {
  let bcSeed = seed.broadcast(like: originalValue)
  return (matmul(bcSeed, right.transposed()), matmul(left.transposed(), bcSeed))
}

extension Tensor {
  @inlinable
  func _adjointTransposed(
    _ permutations: Tensor<Int32>, originalValue: Tensor, seed: Tensor
  ) -> Tensor {
    let seed = seed.broadcast(like: originalValue)
    return seed.transposed(withPermutations: permutations)
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

extension Tensor {
  @inlinable
  func _adjointReshaped(
    toShape newShape: Tensor<Int32>, originalValue: Tensor, seed: Tensor
  ) -> Tensor {
    let seed = seed.broadcast(like: originalValue)
    return seed.reshaped(toShape: shapeTensor)
  }

  @inlinable
  func _adjointExpandingShape(
    at shapeIndex: Int32, originalValue: Tensor, seed: Tensor
  ) -> Tensor {
    let seed = seed.broadcast(like: originalValue)
    return seed.squeezingShape(at: shapeIndex)
  }
}

//===----------------------------------------------------------------------===//
// Normalization
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : BinaryFloatingPoint {
  // TODO: Verify that these calculations are correct.
  @inlinable
  func _adjointBatchNormalized(
    alongAxis axis: Int32,
    offset: Scalar,
    scale: Scalar,
    epsilon: Scalar,
    originalValue: Tensor,
    seed: Tensor
  ) -> (Tensor, Scalar, Scalar) {
    let mean = self.mean(alongAxes: axis)
    let squaredDiff: Tensor = Raw.squaredDifference(self, mean)
    let variance = squaredDiff.mean(alongAxes: axis)

    let diff = self - mean
    let inv = rsqrt(variance + epsilon)
    let norm = diff * inv

    let dNorm = seed * scale
    let dVariance = -(dNorm * diff).sum(alongAxes: axis) / 2 * pow(inv, -3)
    let dMean = (-dNorm * inv).sum(alongAxes: axis) +
      dVariance * (-diff * 2).mean(alongAxes: axis)
    let dOffset = seed.sum(alongAxes: axis)
    let dScale = (norm * seed).sum(alongAxes: axis)
    let dim = Tensor(Tensor<Int32>(shapeTensor[axis]))
    let tmp = (dNorm * inv) + (dVariance * 2 * dMean / dim)
    let dSelf = tmp + (dMean / dim)
    return (dSelf, dOffset.scalarized(), dScale.scalarized())
  }
}

//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : BinaryFloatingPoint {
  /// TensorFlow builtin conv2d gradient helper for the input.
  @inlinable
  @differentiable(
    reverse, wrt: (.1, .2),
    adjoint: _adjointTFConv2DBackpropInput(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropInput(
    shape: Tensor<Int32>,
    filter: Tensor,
    backpropOutput: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return Raw.conv2DBackpropInput(
      inputSizes: shape,
      filter: filter,
      outBackprop: backpropOutput,
      strides: [strides.0, strides.1, strides.2, strides.3],
      padding: padding.raw)
  }

  /// TensorFlow builtin conv2d gradient helper for the filter.
  @inlinable
  @differentiable(
    reverse, wrt: (.0, .2),
    adjoint: _adjointTFConv2DBackpropFilter(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropFilter(
    input: Tensor,
    filterSizes: Tensor<Int32>,
    backpropOutput: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return Raw.conv2DBackpropFilter(
      input,
      filterSizes: filterSizes,
      outBackprop: backpropOutput,
      strides: [strides.0, strides.1, strides.2, strides.3],
      padding: padding.raw)
  }

  @inlinable
  func _adjointTFConv2DBackpropInput(
    _ shape: Tensor<Int32>,
    _ filter: Tensor,
    _ backpropOutput: Tensor,
    _ strides: (Int32, Int32, Int32, Int32),
    _ padding: Padding,
    _ originalValue: Tensor,
    _ seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropFilter(input: seed, filterSizes: shape,
                              backpropOutput: backpropOutput, strides: strides,
                              padding: padding),
      seed.convolved2D(withFilter: filter, strides: strides, padding: padding)
    )
  }

  @inlinable
  func _adjointTFConv2DBackpropFilter(
    _ input: Tensor,
    _ filterSizes: Tensor<Int32>,
    _ backpropOutput: Tensor,
    _ strides: (Int32, Int32, Int32, Int32),
    _ padding: Padding,
    _ originalValue: Tensor,
    _ seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropInput(shape: filterSizes, filter: seed,
                             backpropOutput: backpropOutput, strides: strides,
                             padding: padding),
      input.convolved2D(withFilter: seed, strides: strides, padding: padding)
    )
  }

  @inlinable
  func _adjointConvolved2D(
    filter: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    originalValue: Tensor,
    seed: Tensor
  ) -> (Tensor, Tensor) {
    return (
      _TFConv2DBackpropInput(
        shape: shapeTensor, filter: filter, backpropOutput: seed,
        strides: strides, padding: padding
      ),
      _TFConv2DBackpropFilter(
        input: self, filterSizes: filter.shapeTensor, backpropOutput: seed,
        strides: strides, padding: padding
      )
    )
  }

  @inlinable
  func _adjointMaxPooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    originalValue: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return Raw.maxPoolGradV2(
      origInput: self,
      origOutput: originalValue,
      grad: seed,
      ksize: Tensor<Int32>(kernelSize),
      strides: Tensor<Int32>(strides),
      padding: padding.raw
    )
  }

  @inlinable
  func _adjointAveragePooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    originalValue: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return Raw.avgPoolGrad(
      origInputShape: shapeTensor,
      grad: seed,
      ksize: [kernelSize.0, kernelSize.1, kernelSize.2, kernelSize.3],
      strides: [strides.0, strides.1, strides.2, strides.3],
      padding: padding.raw
    )
  }
}

//===----------------------------------------------------------------------===//
// Composite math
//===----------------------------------------------------------------------===//

@inlinable
func _adjointRelu<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, originalValue: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return Tensor(x > 0) * seed
}
