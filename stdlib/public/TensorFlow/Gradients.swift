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
// - Primal (f): The function being differentiated, or the result of that
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
// '@differentiable(gradient: ...)' is used to define the adjoint for a primal
// function. The automatic differentiation pass will  pick up these adjoints
// and chain them together for arbitrary differentiable programs.
//
// NOTE:
// - Currently, we do not want to expose adjoint functions to users. Each
//   adjoint function's name should start with an underscore.
// FIXME:
// - Handle scalar broadcasting.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Elementwise binary
//===----------------------------------------------------------------------===//

@_inlineable
@_versioned
func _adjointAdd<T : Numeric>(
  _: Tensor<T>, _: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (seed, seed)
}

@inline(never)
@_versioned
func _adjointAdd<T : Numeric>(
  _: Tensor<T>, _: T, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, T) {
  fatalError("Unimplemented")
}

@inline(never)
@_versioned
func _adjointAdd<T : Numeric>(
  _: T, _: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (T, Tensor<T>) {
  fatalError("Unimplemented")
}

@_inlineable
@_versioned
func _adjointSubtract<T : Numeric>(
  _: Tensor<T>, _: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (seed, -seed)
}

@inline(never)
@_versioned
func _adjointSubtract<T : Numeric>(
  _: Tensor<T>, _: T, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, T) {
  fatalError("Unimplemented")
}

@inline(never)
@_versioned
func _adjointSubtract<T : Numeric>(
  _: T, _: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (T, Tensor<T>) {
  fatalError("Unimplemented")
}

@_inlineable
@_versioned
func _adjointMultiply<T : Numeric>(
  _ x: Tensor<T>, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (y * seed, x * seed)
}

@inline(never)
@_versioned
func _adjointMultiply<T : Numeric>(
  _ x: Tensor<T>, _ y: T, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, T) {
  fatalError("Unimplemented")
}

@inline(never)
@_versioned
func _adjointMultiply<T : Numeric>(
  _ x: T, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (T, Tensor<T>) {
  fatalError("Unimplemented")
}

@_inlineable
@_versioned
func _adjointDivide<T : Numeric>(
  _ x: Tensor<T>, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (seed / y, -x / y.squared() * seed)
}

@inline(never)
@_versioned
func _adjointDivide<T : Numeric>(
  _ x: Tensor<T>, _ y: T, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, T) {
  fatalError("Unimplemented")
}

@inline(never)
@_versioned
func _adjointDivide<T : Numeric>(
  _ x: T, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (T, Tensor<T>) {
  fatalError("Unimplemented")
}

@_inlineable
@_versioned
func _adjointMin<T : Numeric & Comparable>(
  _ lhs: Tensor<T>, _ rhs: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  let denom = 1 + Tensor<T>(lhs == rhs)
  let dfdx = seed * Tensor<T>(rhs == primal) / denom
  let dfdy = seed * Tensor<T>(lhs == primal) / denom
  return (dfdx, dfdy)
}

@_inlineable
@_versioned
func _adjointMax<T : Numeric & Comparable>(
  _ lhs: Tensor<T>, _ rhs: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  let denom = 1 + Tensor<T>(lhs == rhs)
  let dfdx = seed * Tensor<T>(lhs == primal) / denom
  let dfdy = seed * Tensor<T>(rhs == primal) / denom
  return (dfdx, dfdy)
}

@_inlineable
@_versioned
func _adjointPow<T : FloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (seed * y * pow(x, y-1), seed * log(x) * primal)
}

//===----------------------------------------------------------------------===//
// Elementwise unary
//===----------------------------------------------------------------------===//

@_inlineable
@_versioned
func _adjointNegate<T : Numeric>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return -seed
}

@_inlineable
@_versioned
func _adjointLog<T : Numeric>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed / x
}

@_inlineable
@_versioned
func _adjointSin<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * cos(x)
}

@_inlineable
@_versioned
func _adjointCos<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return -seed * sin(x)
}

@_inlineable
@_versioned
func _adjointTan<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed / (1 + primal.squared())
}

@_inlineable
@_versioned
func _adjointSinh<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * cosh(x)
}

@_inlineable
@_versioned
func _adjointCosh<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * sinh(x)
}

@_inlineable
@_versioned
func _adjointTanh<T : FloatingPoint>(
  _ x: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed * (1 - primal.squared())
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

@_inlineable
@_versioned
func _adjointDot<T : Numeric>(
  _ x: Tensor<T>, _ y: Tensor<T>, primal: Tensor<T>, seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (seed.dot(y.transposed()), x.transposed().dot(y))
}

@_inlineable
@_versioned
func _adjointTransposed<T : Numeric>(
  _ x: Tensor<T>, _ permutations: Tensor<Int>, primal: Tensor<T>, seed: Tensor<T>
) -> Tensor<T> {
  return seed.transposed(withPermutations: permutations)
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

/// TensorFlow builtin conv2d gradient helper for the input.
@_inlineable
@_versioned
// @differentiable(
//   withRespectTo: (.1, .2),
//   gradient: _adjointTFConv2DBackpropInput(_:_:_:_:_:_:_:)
// )
func _TFConv2DBackpropInput<T : FloatingPoint>(
  shape: Tensor<Int32>,
  filter: Tensor<T>,
  backpropOutput: Tensor<T>,
  strides: [Int],
  padding: Padding
) -> Tensor<T> {
  // FIXME: handle attributes (strides and padding)
  return Tensor(#tfop("Conv2DBackpropInput", "ttt:t",
                      shape.handle, filter.handle, backpropOutput.handle))
}

/// TensorFlow builtin conv2d gradient helper for the filter.
@_inlineable
@_versioned
// @differentiable(
//   withRespectTo: (.0, .2),
//   gradient: _adjointTFConv2DBackpropFilter(_:_:_:_:_:_:_:)
// )
func _TFConv2DBackpropFilter<T : FloatingPoint>(
  input: Tensor<T>,
  filterSizes: Tensor<Int32>,
  backpropOutput: Tensor<T>,
  strides: [Int],
  padding: Padding
) -> Tensor<T> {
  // FIXME: handle attributes (strides and padding)
  return Tensor(#tfop("Conv2DBackpropFilter", "ttt:t",
                      input.handle, filterSizes.handle, backpropOutput.handle))
}

@_inlineable
@_versioned
func _adjointTFConv2DBackpropInput<T : FloatingPoint>(
  _ shape: Tensor<Int32>,
  _ filter: Tensor<T>,
  _ backpropOutput: Tensor<T>,
  _ strides: [Int],
  _ padding: Padding,
  _ primal: Tensor<T>,
  _ seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (
    _TFConv2DBackpropFilter(input: seed, filterSizes: shape,
                            backpropOutput: backpropOutput, strides: strides,
                            padding: padding),
    seed.convolved2D(withFilter: filter, strides: strides, padding: padding)
  )
}

@_inlineable
@_versioned
func _adjointTFConv2DBackpropFilter<T : FloatingPoint>(
  _ input: Tensor<T>,
  _ filterSizes: Tensor<Int32>,
  _ backpropOutput: Tensor<T>,
  _ strides: [Int],
  _ padding: Padding,
  _ primal: Tensor<T>,
  _ seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (
    _TFConv2DBackpropInput(shape: filterSizes, filter: seed,
                           backpropOutput: backpropOutput, strides: strides,
                           padding: padding),
    input.convolved2D(withFilter: seed, strides: strides, padding: padding)
  )
}

@_inlineable
@_versioned
func _adjointConvolved2D<T : FloatingPoint>(
  input: Tensor<T>,
  filter: Tensor<T>,
  strides: [Int],
  padding: Padding,
  primal: Tensor<T>,
  seed: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  return (
    _TFConv2DBackpropInput(shape: input.shapeTensorOriginal, filter: filter,
                           backpropOutput: seed, strides: strides,
                           padding: padding),
    _TFConv2DBackpropFilter(input: input, filterSizes: filter.shapeTensorOriginal,
                            backpropOutput: seed, strides: strides,
                            padding: padding
    )
  )
}

@_inlineable
@_versioned
func _adjointMaxPooled<T>(
  input: Tensor<T>,
  kernelSize: Tensor<Int32>,
  strides: Tensor<Int32>,
  padding: Padding,
  primal: Tensor<T>,
  seed: Tensor<T>
) -> Tensor<T> {
  // TODO: Currently this is not higher order differentiable. Redefine in closed
  // form.
  // FIXME: handle attributes (padding)
  return Tensor(#tfop("MaxPoolGradV2", "ttttt:t",
                      input.shapeTensorOriginal.handle,
                      primal.handle,
                      seed.handle,
                      kernelSize.handle,
                      strides.handle))
}

@_inlineable
@_versioned
func _adjointAveragePooled<T>(
  input: Tensor<T>,
  kernelSize: [Int],
  strides: [Int],
  padding: Padding,
  primal: Tensor<T>,
  seed: Tensor<T>
) -> Tensor<T> {
  // TODO: Currently this is not higher order differentiable. Redefine in closed
  // form.
  // FIXME: handle attributes (ksize, strides, padding)
  return Tensor(#tfop("AvgPoolGrad", "tt:t",
                      input.shapeTensorOriginal.handle,
                      seed.handle))
}
