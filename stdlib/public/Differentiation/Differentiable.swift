//===--- Differentiable.swift ---------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Differentiable protocol, used by the experimental
// differentiable programming project. This API is not stable and subject to
// change.
//
// Please see forum discussion for more information about the differentiable
// programming project:
// https://forums.swift.org/t/differentiable-programming-mega-proposal/28547
//
//===----------------------------------------------------------------------===//

import Swift

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
public protocol Differentiable {
  /// A type representing a differentiable value's derivatives.
  ///
  /// Mathematically, this is equivalent to the tangent bundle of the
  /// differentiable manifold represented by the differentiable type.
  associatedtype TangentVector: Differentiable & AdditiveArithmetic
    where TangentVector.TangentVector == TangentVector

  /// Moves `self` along the given direction. In Riemannian geometry, this is
  /// equivalent to exponential map, which moves `self` on the geodesic surface
  /// along the given tangent vector.
  mutating func move(along direction: TangentVector)

  /// A closure that produces a zero tangent vector, capturing minimal
  /// necessary information from `self`.
  ///
  /// `move(along: zeroTangentVectorInitializer())` should not modify
  /// `self`.
  ///
  /// In some cases, the zero tangent vector of `self` is equal to
  /// `TangentVector.zero`. In other cases, the zero tangent vector depends on
  /// information in `self`, such as shape for an n-dimensional array type.
  /// For differentiable programming, it is more memory-efficient to define a
  /// custom `zeroTangentVectorInitializer` property which returns a closure
  /// that captures and uses only the necessary information to create a zero
  /// tangent vector. For example:
  ///
  ///     struct Vector {
  ///         var scalars: [Float]
  ///         var count: Int { scalars.count }
  ///         init(scalars: [Float]) { ... }
  ///         init(repeating repeatedElement: Float, count: Int) { ... }
  ///     }
  ///
  ///     extension Vector: AdditiveArithmetic { ... }
  ///
  ///     extension Vector: Differentiable {
  ///         typealias TangentVector = Vector
  ///
  ///         @noDerivative
  ///         var zeroTangentVectorInitializer: () -> TangentVector {
  ///             let count = self.count
  ///             return { TangentVector(repeating: 0, count: count) }
  ///         }
  ///     }
  var zeroTangentVectorInitializer: () -> TangentVector { get }
}

public extension Differentiable where TangentVector == Self {
  @_alwaysEmitIntoClient
  mutating func move(along direction: TangentVector) {
    self += direction
  }
}

public extension Differentiable {
  /// A tangent vector initialized using `zeroTangentVectorInitializer`.
  /// `move(along: zeroTangentVector)` should not modify `self`.
  var zeroTangentVector: TangentVector { zeroTangentVectorInitializer() }
}
