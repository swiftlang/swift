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
///         // struct Parameters : Differentiable {
///         //     var w: Tensor2D<Float>
///         //     var b: Tensor1D<Float>
///         //
///         //     var parameters: Parameters {
///         //         get {
///         //             return Parameters(w: w, b: b)
///         //         }
///         //         set {
///         //             w = newValue.w
///         //             b = newValue.b
///         //         }
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
public protocol Learnable : DifferentiableModule
  where Input : Differentiable,
        Output : Differentiable,
        Parameters : Differentiable {
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
