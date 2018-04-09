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
// '@differentiable(reverse, adjoint: ...)' is used to define the adjoint for a partial
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
//   type-checked, define constraints on FloatingPoint in primal declarations
//   and define adjoints on FloatingPoint.
//
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
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed.unbroadcast(to: x), seed.unbroadcast(to: y))
  }

  @_inlineable @_versioned
  static func _adjointSubtract(
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return (seed.unbroadcast(to: x), 0 - seed.unbroadcast(to: y))
  }

  @_inlineable @_versioned
  static func _adjointMultiply(
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return ((y * seed).unbroadcast(to: x),
            (x * seed).unbroadcast(to: y))
  }

  @_inlineable @_versioned
  static func _adjointDivide(
    _ x: Self, _ y: Self, partial: Self, seed: Self
  ) -> (Self, Self) {
    return ((seed / y).unbroadcast(to: x),
            ((0 - x) / y.squared() * seed).unbroadcast(to: y))
  }
}

@_inlineable @_versioned
func _adjointMin<T : TensorProtocol>(
  _ x: T, _ y: T, partial: T, seed: T
) -> (T, T) where T.Scalar : Numeric & Comparable {
  let denom = 1 + T(x.elementsEqual(y))
  let dfdx = seed * T(y.elementsEqual(partial)) / denom
  let dfdy = seed * T(x.elementsEqual(partial)) / denom
  return (dfdx.unbroadcast(to: x), dfdy.unbroadcast(to: y))
}

@_inlineable @_versioned
func _adjointMax<T : TensorProtocol>(
  _ x: T, _ y: T, partial: T, seed: T
) -> (T, T) where T.Scalar : Numeric & Comparable {
  let denom = 1 + T(x.elementsEqual(y))
  let dfdx = seed * T(x.elementsEqual(partial)) / denom
  let dfdy = seed * T(y.elementsEqual(partial)) / denom
  return (dfdx.unbroadcast(to: x), dfdy.unbroadcast(to: y))
}

@_inlineable @_versioned
func _adjointPow<T : TensorProtocol>(
  _ x: T, _ y: T, partial: T, seed: T
) -> (T, T) where T.Scalar : FloatingPoint {
  return ((seed * y * pow(x, y-1)).unbroadcast(to: x),
          (seed * log(x) * partial).unbroadcast(to: y))
}

//===----------------------------------------------------------------------===//
// Elementwise unary
//===----------------------------------------------------------------------===//

extension TensorProtocol where Scalar : SignedNumeric {
  @_inlineable @_versioned
  static func _adjointNegate(_ x: Self, partial: Self, seed: Self) -> Self {
    return -seed
  }
}

@_inlineable @_versioned
func _adjointLog<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed / x
}

@_inlineable @_versioned
func _adjointSin<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed * cos(x)
}

@_inlineable @_versioned
func _adjointCos<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return -seed * cos(x)
}

@_inlineable @_versioned
func _adjointTan<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed / (1 + partial.squared())
}

@_inlineable @_versioned
func _adjointSinh<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed * cosh(x)
}

@_inlineable @_versioned
func _adjointCosh<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed * sinh(x)
}

@_inlineable @_versioned
func _adjointTanh<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed * (1 - partial.squared())
}

@_inlineable @_versioned
func _adjointExp<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed
}

@_inlineable @_versioned
func _adjointSqrt<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return seed / (2 * partial)
}

@_inlineable @_versioned
func _adjointRsqrt<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return -seed / 2 * pow(partial, 3)
}

func _adjointSquared<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
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
// Normalization
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : BinaryFloatingPoint {
  // TODO: Verify that these calculations are correct.
  @_inlineable @_versioned
  func _adjointBatchNormalized(
    alongAxis axis: Int32,
    offset: Tensor = Tensor(0),
    scale: Tensor = Tensor(1),
    epsilon: Tensor = Tensor(0.001),
    partial: Tensor,
    seed: Tensor
  ) -> (Tensor, Tensor, Tensor) {
    let mean = self.mean(alongAxes: axis)
    let squaredDiff: Tensor = #tfop("SquaredDifference", self, mean)
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
    // NOTE: a temporary variable is necessary here to help the type checker.
    // Otherwise, the following error occurs: "the compiler is unable to
    // type-check this expression in reasonable time".
    let tmp = (dNorm * inv) + (dVariance * 2 * dMean / dim)
    let dSelf = tmp + (dMean / dim)
    return (dSelf, dOffset, dScale)
  }
}

//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : FloatingPoint {
  /// TensorFlow builtin conv2d gradient helper for the input.
  @_inlineable @_versioned
  @differentiable(
    reverse, withRespectTo: (.1, .2),
    adjoint: _adjointTFConv2DBackpropInput(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropInput(
    shape: Tensor<Int32>,
    filter: Tensor,
    backpropOutput: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return #tfop("Conv2DBackpropInput", shape, filter, backpropOutput,
                 strides: [strides.0, strides.1, strides.2, strides.3],
                 padding: padding.cName)
  }

  /// TensorFlow builtin conv2d gradient helper for the filter.
  @_inlineable @_versioned
  @differentiable(
    reverse, withRespectTo: (.0, .2),
    adjoint: _adjointTFConv2DBackpropFilter(_:_:_:_:_:_:_:)
  )
  func _TFConv2DBackpropFilter(
    input: Tensor,
    filterSizes: Tensor<Int32>,
    backpropOutput: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return #tfop("Conv2DBackpropFilter", input, filterSizes, backpropOutput,
                 strides: [strides.0, strides.1, strides.2, strides.3],
                 padding: padding.cName)
  }

  @_inlineable @_versioned
  func _adjointTFConv2DBackpropInput(
    _ shape: Tensor<Int32>,
    _ filter: Tensor,
    _ backpropOutput: Tensor,
    _ strides: (Int32, Int32, Int32, Int32),
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
    _ strides: (Int32, Int32, Int32, Int32),
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
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    partial: Tensor,
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

  @_inlineable @_versioned
  func _adjointMaxPooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    partial: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return #tfop(
      "MaxPoolGradV2", shapeTensor, partial, seed, Tensor<Int32>(kernelSize),
      Tensor<Int32>(strides), padding: padding.cName
    )
  }

  @_inlineable @_versioned
  func _adjointAveragePooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding,
    partial: Tensor,
    seed: Tensor
  ) -> Tensor {
    // TODO: Currently this is not higher order differentiable. Redefine in
    // closed form.
    return #tfop(
      "AvgPoolGrad", shapeTensor, seed,
      ksize: [kernelSize.0, kernelSize.1, kernelSize.2, kernelSize.3],
      strides: [strides.0, strides.1, strides.2, strides.3],
      padding: padding.cName
    )
  }
}

//===----------------------------------------------------------------------===//
// Composite math
//===----------------------------------------------------------------------===//

@_inlineable @_versioned
func _adjointRelu<T : TensorProtocol>(
  _ x: T, partial: T, seed: T
) -> T where T.Scalar : FloatingPoint {
  return T(x > 0) * seed
}
