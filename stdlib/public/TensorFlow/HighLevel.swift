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
// This file defines high-level neural network APIs.
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
  associatedtype Parameters
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
  /// Returns the gradient with respect to an input and the instance's
  /// parameters, backpropagating an adjoint value.
  func gradient(
    for input: Input, backpropagating adjoint: Output
  ) -> (Input, Parameters)
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
  associatedtype Trainee : Learnable

  /// The learning rate for gradient descent.
  var learningRate: Trainee.Parameters.DifferentiationCurrency { get set }

  /// Optimizes the parameters of a `Trainee` instance given the gradient of the
  /// trainee's parameters.
  func optimize(
    _ instance: inout Trainee.Parameters,
    gradient: Trainee.Parameters
  )
}

//===----------------------------------------------------------------------===//
//
// Convolution layer
//
//===----------------------------------------------------------------------===//

/// A layer with a 4-D kernel that computes 2-D convolutions given 4-D inputs.
public struct Convolution2DLayer<Scalar> : DifferentiableModule
  where Scalar : FloatingPoint & AccelerableByTensorFlow {
  /// A 4-D filter tensor.
  // TODO: To be marked with @parameter.
  public var filter: Tensor<Scalar>

  /// The strides of the sliding window for each dimension of a 4-D input.
  /// `strides.count` must equal 4 and `strides[0]` and `strides[3]` must equal
  /// 1.
  public let strides: [Int32]

  /// The padding algorithm for convolution.
  public let padding: Padding

  /// Creates a new instance with the specified filter, strides, and padding.
  // NOTE: Swift generates default memberwise initializers, but they have
  // internal access, not public. Public initializers must be manually declared.
  // TODO: Consider adding default argument values? (eg. strides = [1, 1, 1, 1])
  // Default values might not work out of the box (send/receive errors).
  @_inlineable @inline(__always)
  public init(filter: Tensor<Scalar>, strides: [Int32], padding: Padding) {
    self.filter = filter
    self.strides = strides
    self.padding = padding
  }

  // TODO: The `Parameters` struct type and the `parameters` stored property
  // will be compiler synthesized. Remove their explicit declarations when
  // compiler synthesization is implemented.
  public struct Parameters : Differentiable {
    // The currency type of differentiation. This will be compiler synthesized
    // to be the currency type of the stored properties with least precision.
    // The currency type is important for initializing intermediate values
    // during automatic differentiation, such as the initial adjoint/tangent and
    // the seed.
    public typealias DifferentiationCurrency = Scalar

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

  /// Computes a 2-D convolution Tensor given a 4-D input Tensor, using the 4-D
  /// filter Tensor.
  @_inlineable @inline(__always)
  public func applied(to input: Tensor<Scalar>) -> Tensor<Scalar> {
    return input.convolved2D(
      withFilter: filter, strides: strides, padding: padding
    )
  }

  /// Computes the gradient of a 2-D convolution Tensor with respect to a 4-D
  /// input Tensor and the 4-D filter Tensor. This can be automatically computed
  /// after more compiler support for automatic differentiation is added.
  @_inlineable @inline(__always)
  public func gradient(
    for input: Tensor<Scalar>, backpropagating adjoint: Tensor<Scalar>
  ) -> (Tensor<Scalar>, Parameters) {
    let partial = applied(to: input)
    let (dInput, dFilter) = input._adjointConvolved2D(
      filter: filter,
      strides: strides,
      padding: padding,
      partial: partial,
      seed: adjoint.broadcast(to: partial)
    )
    return (dInput, Parameters(filter: dFilter))
  }
}

//===----------------------------------------------------------------------===//
//
// Fully connected layer
//
//===----------------------------------------------------------------------===//

/// A fully-connected layer with weight and bias tensors. Fully-connected layers
/// apply a linear transformation to inputs.
// TODO:
// - Add initializers that take in input/output sizes and initialization
//   strategies for weight/bias (ones, zeros, random uniform, etc).
public struct FullyConnectedLayer<Scalar> : DifferentiableModule
  where Scalar : FloatingPoint & AccelerableByTensorFlow {
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
  public struct Parameters : Differentiable {
    // The currency type of differentiation. This will be compiler synthesized
    // to be the currency type of the stored properties with least precision.
    // The currency type is important for initializing intermediate values
    // during automatic differentiation, such as the initial adjoint/tangent and
    // the seed.
    public typealias DifferentiationCurrency = Scalar

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

  /// Computes the operation `dot(input, weight) + bias`.
  @_inlineable @inline(__always)
  public func applied(to input: Tensor<Scalar>) -> Tensor<Scalar> {
    return input.dot(weight) + bias
  }

  /// Computes the gradient of a 2-D convolution Tensor with respect to a 4-D
  /// input Tensor and the 4-D filter Tensor. This can be automatically computed
  /// after more compiler support for automatic differentiation is added.
  @_inlineable @inline(__always)
  public func gradient(
    for input: Tensor<Scalar>, backpropagating adjoint: Tensor<Scalar>
  ) -> (Tensor<Scalar>, Parameters) {
    // Broadcast adjoint to correct shape. AD should do this automatically when
    // implemented.
    // NOTE: proper AD would require an `unbroadcast` op for _adjointAdd. There
    // is a manually workaround here.
    let dot = input.dot(weight) + bias
    let dBias = adjoint.broadcast(to: bias)
    let dDot = adjoint.broadcast(to: dot)
    let (dInput, dWeight) = input._adjointDot(weight, partial: dot, seed: dDot)
    return (dInput, Parameters(weight: dWeight, bias: dBias))
  }
}
