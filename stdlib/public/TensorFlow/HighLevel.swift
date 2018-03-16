//===-- HighLevel.swift ---------------------------------------*- swift -*-===//
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
// This file defines experimental high-level neural network APIs.
//
//===----------------------------------------------------------------------===//

// TODO:
// - Add/improve documentation.
// - Implement @parameter attribute, add compiler support for synthesized
//   Parameters struct and `parameters` variable.

//===----------------------------------------------------------------------===//
// Parameterized
//===----------------------------------------------------------------------===//

/// A type whose values have parameters.
///
/// Instances of `Parameterized` types have parameters, represented as stored
/// properties. Stored properties are marked as parameters with the `@parameter`
/// attribute. The Swift compiler automatically generates a member struct type
/// `Parameters`, which includes all of the marked properties.
///
public protocol Parameterized {
  /// The type representing all parameters, synthesized from stored properties
  /// marked with `@parameter`.
  associatedtype Parameters : ParameterAggregate

  /// A synthesized instance of Parameters.
  var parameters: Parameters { get set }
}

//===----------------------------------------------------------------------===//
// Module
//===----------------------------------------------------------------------===//

/// A neural network module.
///
/// Types that conform to `Module` represent functions that map inputs to
/// outputs. They may have an internal state represented by parameters, such as
/// weight tensors.
///
/// `Module` instances define an `applied` method for mapping inputs to outputs.
public protocol Module : Parameterized {
  /// The input type of the module.
  associatedtype Input
  /// The output type of the module.
  associatedtype Output

  /// Returns the output obtained from applying to an input.
  func applied(to input: Input) -> Output
}

//===----------------------------------------------------------------------===//
// Differentiation tape protocol (to be removed when AD is implemented)
//===----------------------------------------------------------------------===//
public protocol DifferentiationTapeProtocol {
  associatedtype DifferentiationTapeResult
  var result: DifferentiationTapeResult { get }
}

//===----------------------------------------------------------------------===//
// DifferentiableModule
//===----------------------------------------------------------------------===//

/// A differentiable neural network module.
///
/// Types that conform to `DifferentiableModule` represent differentiable
/// functions that map inputs to outputs. The `Input`, `Output`, and
/// `Parameters` associated types for `DifferentiableModule` must all conform to
/// `Differentiable`.
///
/// `DifferentiableModule` instances define a `gradient` method that computes
/// the gradient with respect to an input and the instance's parameters.
public protocol DifferentiableModule : Module
  where Input : Differentiable,
        Output : Differentiable,
        Parameters : Differentiable {
  associatedtype DifferentiationPrimalValues : DifferentiationTapeProtocol
    where DifferentiationPrimalValues.DifferentiationTapeResult == Output

  func primal(for input: Input) -> DifferentiationPrimalValues
  func adjoint(for input: Input,
               with primalValues: DifferentiationPrimalValues,
               backpropagating seed: Output) -> (Input, Parameters)
}

public extension DifferentiableModule {
  @_inlineable @inline(__always)
  func applied(to input: Input) -> Output {
    return primal(for: input).result
  }

  /// Returns the gradient with respect to an input and the instance's
  /// parameters, backpropagating an adjoint value.
  @_inlineable @inline(__always)
  func gradient(
    for input: Input, backpropagating adjoint: Output
  ) -> (Input, Parameters) {
    return self.adjoint(for: input,
                        with: primal(for: input),
                        backpropagating: adjoint)
  }
}

//===----------------------------------------------------------------------===//
// Learnable
//===----------------------------------------------------------------------===//

/// A differentiable neural network module that defines a loss function.
///
/// Types that conform to `Learnable` define a `loss` function that computes the
/// loss of a predicted output from an expected output.
///
/// Example:
///
///     public struct Perceptron : Learnable {
///         @parameter var w: Tensor2D<Float>
///         @parameter var b: Tensor1D<Float>
///
///         // The synthesized `Parameters` struct is:
///         // public struct Parameters : Differentiable {
///         //     public var w: Tensor2D<Float>
///         //     public var b: Tensor1D<Float>
///         // }
///         //
///         // public var parameters: Parameters {
///         //     get {
///         //         return Parameters(w: w, b: b)
///         //     }
///         //     set {
///         //         w = newValue.w
///         //         b = newValue.b
///         //     }
///         // }
///
///         func applied(to input: Tensor2D<Float>) -> Tensor2D<Float> {
///             return input.dot(w) + b.rankLifted()
///         }
///
///         func gradient(
///             for input: Tensor2D<Float>,
///             backpropagating adjoint: Tensor2D<Float>
///         ) -> (Tensor2D, Parameters) {
///             let dInput = w.transposed().dot(seed)
///             let dW = seed.transposed().dot(input)
///             let dB = seed.sum(alongAxis: 0)
///             return (dInput, Parameters(w: dW, b: dB))
///         }
///
///         func loss(
///             of predicted: Tensor2D<Float>,
///             from expected: Tensor2D<Float>
///         ) -> Float {
///             return (predicted - expected).squared()
///                 .mean(alongAxes: [0, 1]).scalarized()
///         }
///     }
///
public protocol Learnable : DifferentiableModule {
  associatedtype Loss : FloatingPoint

  /// Returns the loss of a predicted output from an expected output.
  func loss(of predicted: Output, from expected: Output) -> Loss
}

//===----------------------------------------------------------------------===//
// Optimizer
//===----------------------------------------------------------------------===//

/// A reference type whose instances can optimize `Learnable` trainees.
///
/// `Optimizer` instances can optimize `Trainee` instances given the gradient of
/// the trainee's parameters.
public protocol Optimizer : AnyObject {
  associatedtype Scalar : BinaryFloatingPoint

  /// The learning rate for gradient descent.
  var learningRate: Scalar { get }

  /// Optimizes the parameters of a `Trainee` instance given the gradient of the
  /// trainee's parameters.
  func optimize<P : ParameterAggregate>(
    _ parameters: inout P,
    gradient: P
  ) where P.Scalar == Scalar
}

//===----------------------------------------------------------------------===//
// Common optimizers
//===----------------------------------------------------------------------===//

@_fixed_layout
public final class StochasticGradientDescent<Scalar> : Optimizer
  where Scalar : BinaryFloatingPoint {
  public let learningRate: Scalar
  // NOTE: To be implemented later.
  // public let momentum: Trainee.Parameters.Scalar = 0
  // public let decay: Trainee.Parameters.Scalar = 0
  // public let isNesterovMomentum: Bool = false
  // public private(set) var epoch: Int32 = 0

  @_inlineable @inline(__always)
  public init(learningRate: Scalar) {
    self.learningRate = learningRate
  }

  @_inlineable @inline(__always)
  public func optimize<P : ParameterAggregate>(
    _ parameters: inout P,
    gradient: P
  ) where P.Scalar == Scalar {
    parameters.update(with: gradient) { param, grad in
      param -= grad * learningRate
    }
  }
}

//===----------------------------------------------------------------------===//
// Convolution layer
//===----------------------------------------------------------------------===//

/// A layer with a 4-D kernel that computes 2-D convolutions given 4-D inputs.
@_fixed_layout
public struct Convolution2DLayer<Scalar> : DifferentiableModule
  where Scalar : FloatingPoint & AccelerableByTensorFlow {

  public typealias Input = Tensor<Scalar>
  public typealias Output = Tensor<Scalar>

  /// A 4-D filter tensor.
  // TODO: To be marked with @parameter.
  public var filter: Tensor<Scalar>

  /// The strides of the sliding window for each dimension of a 4-D input.
  /// Strides in non-spatial dimensions must be 1.
  public let strides: (Int32, Int32, Int32, Int32)

  /// The padding algorithm for convolution.
  public let padding: Padding

  /// Creates a new instance with the specified filter, strides, and padding.
  // NOTE: Swift generates default memberwise initializers, but they have
  // internal access, not public. Public initializers must be manually declared.
  // TODO: Consider adding default argument values? (eg. strides = (1, 1, 1, 1))
  // Default values might not work out of the box (send/receive errors).
  @_inlineable @inline(__always)
  public init(filter: Tensor<Scalar>, strides: (Int32, Int32, Int32, Int32),
              padding: Padding) {
    self.filter = filter
    self.strides = strides
    self.padding = padding
  }

  @_inlineable @inline(__always)
  public init(filterShape: TensorShape, strides: (Int32, Int32, Int32, Int32),
              padding: Padding) {
    self.init(filter: Tensor(randomNormal: filterShape),
              strides: strides,
              padding: padding)
  }

  // TODO: The `Parameters` struct type and the `parameters` stored property
  // will be compiler synthesized. Remove their explicit declarations when
  // compiler synthesization is implemented.
  @_fixed_layout
  public struct Parameters : Differentiable, ParameterAggregate {
    // The currency type of differentiation. This will be compiler synthesized
    // to be the currency type of the stored properties with least precision.
    // The currency type is important for initializing intermediate values
    // during automatic differentiation, such as the initial adjoint/tangent and
    // the seed.
    public typealias DifferentiationCurrency = Scalar

    public typealias Parameter = Tensor<Scalar>

    // Synthesized property. `filter` will be synthesized in `Parameters`
    // because it will be marked with `@parameter`.
    public var filter: Tensor<Scalar>

    // An initializer which sets the values for each parameter.
    @_inlineable @inline(__always)
    public init(filter: Tensor<Scalar>) {
      self.filter = filter
    }

    // This initializer is a `Differentiable` requirement and will be
    // compiler synthesized.
    @_inlineable @inline(__always)
    public init(numericallyBroadcasting value: Scalar, to other: Parameters) {
      self.filter = Tensor<Scalar>(shape: other.filter.shape, repeating: value)
    }

    // This operator is a `Differentiable` requirement and will be compiler
    // synthesized.
    @_inlineable @inline(__always)
    public static func + (lhs: Parameters, rhs: Parameters) -> Parameters {
      return Parameters(
        filter: lhs.filter + rhs.filter
      )
    }

    @_inlineable @inline(__always)
    public mutating func update(
      with gradient: Parameters,
      by updateParameter: (inout Parameter, Parameter) -> Void
    ) {
      updateParameter(&filter, gradient.filter)
    }
  }

  /// An instance of `Parameters`. This will be synthesized.
  @_inlineable
  public var parameters: Parameters {
    @inline(__always)
    get {
      return Parameters(filter: filter)
    }
    @inline(__always)
    set {
      filter = newValue.filter
    }
  }

  @_fixed_layout
  public struct DifferentiationPrimalValues : DifferentiationTapeProtocol {
    public let result: Tensor<Scalar>
    @_versioned @_inlineable
    init(result: Tensor<Scalar>) {
      self.result = result
    }
  }

  /// Computes a 2-D convolution Tensor given a 4-D input Tensor, using the 4-D
  /// filter Tensor.
  @_inlineable @inline(__always)
  public func primal(
    for input: Tensor<Scalar>
  ) -> DifferentiationPrimalValues {
    let result = input.convolved2D(
      withFilter: filter, strides: strides, padding: padding
    )
    return DifferentiationPrimalValues(result: result)
  }

  /// Computes the gradient of a 2-D convolution Tensor with respect to a 4-D
  /// input Tensor and the 4-D filter Tensor. This can be automatically computed
  /// after more compiler support for automatic differentiation is added.
  @_inlineable @inline(__always)
  public func adjoint(
    for input: Tensor<Scalar>,
    with primalValues: DifferentiationPrimalValues,
    backpropagating adjoint: Tensor<Scalar>
  ) -> (Tensor<Scalar>, Parameters) {
    let (dInput, dFilter) = input._adjointConvolved2D(
      filter: filter,
      strides: strides,
      padding: padding,
      partial: primalValues.result,
      seed: adjoint.broadcast(to: primalValues.result)
    )
    return (dInput, Parameters(filter: dFilter))
  }
}

//===----------------------------------------------------------------------===//
// Fully connected layer
//===----------------------------------------------------------------------===//

/// A fully-connected layer with weight and bias tensors. Fully-connected layers
/// apply a linear transformation to inputs.
// TODO:
// - Add initializers that take in input/output sizes and initialization
//   strategies for weight/bias (ones, zeros, random uniform, etc).
@_fixed_layout
public struct FullyConnectedLayer<Scalar> : DifferentiableModule
  where Scalar : FloatingPoint & AccelerableByTensorFlow {

  public typealias Input = Tensor<Scalar>
  public typealias Output = Tensor<Scalar>

  /// The weight tensor.
  // TODO: To be marked with @parameter.
  public var weight: Tensor<Scalar>

  /// The bias tensor.
  // TODO: To be marked with @parameter.
  public var bias: Tensor<Scalar>

  /// Creates a new instance with the specified weight and bias.
  // NOTE: Swift generates default memberwise initializers, but they have
  // internal access, not public. Public initializers must be manually declared.
  @_inlineable @inline(__always)
  public init(weight: Tensor<Scalar>,
              bias: Tensor<Scalar>) {
    self.weight = weight
    self.bias = bias
  }

  // TODO: The `Parameters` struct type and the `parameters` stored property
  // will be compiler synthesized. Remove their explicit declarations when
  // compiler synthesization is implemented.
  @_fixed_layout
  public struct Parameters : Differentiable, ParameterAggregate {
    // The currency type of differentiation. This will be compiler synthesized
    // to be the currency type of the stored properties with least precision.
    // The currency type is important for initializing intermediate values
    // during automatic differentiation, such as the initial adjoint/tangent and
    // the seed.
    public typealias DifferentiationCurrency = Scalar

    public typealias Parameter = Tensor<Scalar>

    // Synthesized properties. `weight` and `bias` will be synthesized in
    // `Parameters` because they will be marked with `@parameter`.
    public var weight: Tensor<Scalar>
    public var bias: Tensor<Scalar>

    // An initializer which sets the values for each parameter.
    @_inlineable @inline(__always)
    public init(weight: Tensor<Scalar>, bias: Tensor<Scalar>) {
      self.weight = weight
      self.bias = bias
    }

    // This initializer is a `Differentiable` requirement and will be
    // compiler synthesized.
    @_inlineable @inline(__always)
    public init(numericallyBroadcasting value: Scalar, to other: Parameters) {
      self.weight = Tensor<Scalar>(shape: other.weight.shape, repeating: value)
      self.bias = Tensor<Scalar>(shape: other.bias.shape, repeating: value)
    }

    // This operator is a `Differentiable` requirement and will be compiler
    // synthesized.
    @_inlineable @inline(__always)
    public static func + (lhs: Parameters, rhs: Parameters) -> Parameters {
      return Parameters(
        weight: lhs.weight + rhs.weight,
        bias: lhs.bias + rhs.bias
      )
    }

    @_inlineable @inline(__always)
    public mutating func update(
      with gradient: Parameters,
      by updateParameter: (inout Parameter, Parameter) -> Void
    ) {
      updateParameter(&weight, gradient.weight)
      updateParameter(&bias, gradient.bias)
    }
  }

  /// An instance of `Parameters`. This will be synthesized.
  @_inlineable
  public var parameters: Parameters {
    @inline(__always)
    get {
      return Parameters(weight: weight, bias: bias)
    }
    @inline(__always)
    set {
      weight = newValue.weight
      bias = newValue.bias
    }
  }

  @_fixed_layout
  public struct DifferentiationPrimalValues : DifferentiationTapeProtocol {
    @_versioned let dot: Tensor<Scalar>
    public let result: Tensor<Scalar>
    @_versioned @_inlineable
    init(dot: Tensor<Scalar>, result: Tensor<Scalar>) {
      self.dot = dot
      self.result = result
    }
  }

  /// Computes the operation `dot(input, weight) + bias`.
  @_inlineable @inline(__always)
  public func primal(for input: Tensor<Scalar>) -> DifferentiationPrimalValues {
    let dot = input.dot(weight)
    let add = dot + bias
    return DifferentiationPrimalValues(dot: dot, result: add)
  }

  /// Computes the gradient of a 2-D convolution Tensor with respect to a 4-D
  /// input Tensor and the 4-D filter Tensor. This can be automatically computed
  /// after more compiler support for automatic differentiation is added.
  @_inlineable @inline(__always)
  public func adjoint(
    for input: Tensor<Scalar>,
    with primalValues: DifferentiationPrimalValues,
    backpropagating adjoint: Tensor<Scalar>
  ) -> (Tensor<Scalar>, Parameters) {
    // NOTE: proper AD would require an `unbroadcast` op for _adjointAdd. There
    // is a manual workaround here.
    let dot = primalValues.dot
    let dBias = adjoint.broadcast(to: bias)
    let dDot = adjoint.broadcast(to: dot)
    let (dInput, dWeight) = input._adjointDot(weight, partial: dot, seed: dDot)
    return (dInput, Parameters(weight: dWeight, bias: dBias))
  }
}

//===----------------------------------------------------------------------===//
// Normalization layers
//===----------------------------------------------------------------------===//

/// A batch normalization layer that transforms inputs to have a mean close to 0
/// and standard deviation close to 1. Batch normalization layers have a running
/// mean, a running variance, and a momentum. They may optionally have an
/// offset, a scale, and a variance epsilon.
// TODO: Handle running mean/variance.
@_fixed_layout
public struct BatchNormalizationLayer<Scalar> : DifferentiableModule
  where Scalar : BinaryFloatingPoint & AccelerableByTensorFlow {

  public typealias Input = Tensor<Scalar>
  public typealias Output = Tensor<Scalar>

  /// The axis for batch normalization.
  public let axis: Int32

  /// The momentum for the running mean and running variance.
  public let momentum: Tensor<Scalar>

  /// The offset value.
  // TODO: To be marked with @parameter.
  public var offset: Tensor<Scalar>

  /// The scale value.
  // TODO: To be marked with @parameter.
  public var scale: Tensor<Scalar>

  /// The variance epsilon value.
  public let epsilon: Tensor<Scalar>

  /// The running mean.
  public var runningMean: Tensor<Scalar>

  /// The running variance.
  public var runningVariance: Tensor<Scalar>

  /// Creates a new instance with the specified weight and bias.
  // NOTE: Swift generates default memberwise initializers, but they have
  // internal access, not public. Public initializers must be manually declared.
  @_inlineable @inline(__always)
  public init(axis: Int32,
              momentum: Tensor<Scalar> = Tensor(0.99),
              offset: Tensor<Scalar> = Tensor(0),
              scale: Tensor<Scalar> = Tensor(0),
              epsilon: Tensor<Scalar> = Tensor(0.001)) {
    self.axis = axis
    self.momentum = momentum
    self.offset = offset
    self.scale = scale
    self.epsilon = epsilon
    /// Initialize running mean and variance with dummy values.
    self.runningMean = Tensor(0)
    self.runningVariance = Tensor(1)
  }

  // TODO: The `Parameters` struct type and the `parameters` stored property
  // will be compiler synthesized. Remove their explicit declarations when
  // compiler synthesization is implemented.
  @_fixed_layout
  public struct Parameters : Differentiable, ParameterAggregate {
    // The currency type of differentiation. This will be compiler synthesized
    // to be the currency type of the stored properties with least precision.
    // The currency type is important for initializing intermediate values
    // during automatic differentiation, such as the initial adjoint/tangent and
    // the seed.
    public typealias DifferentiationCurrency = Scalar

    public typealias Parameter = Tensor<Scalar>

    // Synthesized properties. `offset` and `scale` will be synthesized in
    // `Parameters` because they will be marked with `@parameter`.
    public var offset: Tensor<Scalar>
    public var scale: Tensor<Scalar>

    // An initializer which sets the values for each parameter.
    @_inlineable @inline(__always)
    public init(offset: Tensor<Scalar>, scale: Tensor<Scalar>) {
      self.offset = offset
      self.scale = scale
    }

    // This initializer is a `Differentiable` requirement and will be
    // compiler synthesized.
    @_inlineable @inline(__always)
    public init(numericallyBroadcasting value: Scalar, to other: Parameters) {
      self.offset = Tensor<Scalar>(shape: other.offset.shape, repeating: value)
      self.scale = Tensor<Scalar>(shape: other.scale.shape, repeating: value)
    }

    // This operator is a `Differentiable` requirement and will be compiler
    // synthesized.
    @_inlineable @inline(__always)
    public static func + (lhs: Parameters, rhs: Parameters) -> Parameters {
      return Parameters(
        offset: lhs.offset + rhs.offset,
        scale: lhs.scale + rhs.scale
      )
    }

    @_inlineable @inline(__always)
    public mutating func update(
      with gradient: Parameters,
      by updateParameter: (inout Parameter, Parameter) -> Void
    ) {
      updateParameter(&offset, gradient.offset)
      updateParameter(&scale, gradient.scale)
    }
  }

  /// An instance of `Parameters`. This will be synthesized.
  @_inlineable
  public var parameters: Parameters {
    @inline(__always)
    get {
      return Parameters(offset: offset, scale: scale)
    }
    @inline(__always)
    set {
      offset = newValue.offset
      scale = newValue.scale
    }
  }

  @_fixed_layout
  public struct DifferentiationPrimalValues : DifferentiationTapeProtocol {
    public let result: Tensor<Scalar>
    @_versioned @_inlineable
    init(result: Tensor<Scalar>) {
      self.result = result
    }
  }

  /// Computes the batch normalization operation.
  // TODO: Handle running mean/variance.
  @_inlineable @inline(__always)
  public func primal(
    for input: Tensor<Scalar>
  ) -> DifferentiationPrimalValues {
    let result = input.batchNormalized(alongAxis: axis, offset: offset,
                                       scale: scale, epsilon: epsilon)
    return DifferentiationPrimalValues(result: result)
  }

  /// Computes the gradient of the batch normalization operation. This can be
  /// automatically computed after more compiler support for automatic
  /// differentiation is added.
  @_inlineable @inline(__always)
  public func adjoint(
    for input: Tensor<Scalar>,
    with primalValues: DifferentiationPrimalValues,
    backpropagating adjoint: Tensor<Scalar>
  ) -> (Tensor<Scalar>, Parameters) {
    // NOTE: Adjoint is manually broadcasted to correct shape. AD should do this
    // automatically when implemented.
    let adjoint = adjoint.broadcast(to: primalValues.result)
    let (dInput, dOffset, dScale) = input._adjointBatchNormalized(
      alongAxis: axis, offset: offset, scale: scale, epsilon: epsilon,
      partial: primalValues.result, seed: adjoint
    )
    return (dInput, Parameters(offset: dOffset, scale: dScale))
  }
}
