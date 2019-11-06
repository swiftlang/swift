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

  // SWIFT_ENABLE_TENSORFLOW
  /// A tangent vector such that `move(along: zeroTangentVector)` will not
  /// modify `self`.
  /// - Note: `zeroTangentVector` can be `TangentVector.zero` in most cases,
  ///   but types whose tangent vectors depend on instance properties of `self`
  ///   need to provide a different implementation. For example, the tangent
  ///   vector of an `Array` depends on the array's `count`.
  @available(*, deprecated, message: """
      `zeroTangentVector` derivation has not been implemented; do not use \
      this property
      """)
  var zeroTangentVector: TangentVector { get }
  // SWIFT_ENABLE_TENSORFLOW END
}

public extension Differentiable where TangentVector == Self {
  @_alwaysEmitIntoClient
  mutating func move(along direction: TangentVector) {
    self += direction
  }
}

// SWIFT_ENABLE_TENSORFLOW
public extension Differentiable {
  // This is a temporary solution that allows us to add `zeroTangentVector`
  // without implementing derived conformances. This property is marked
  // unavailable because it will produce incorrect results when tangent vectors
  // depend on instance properties of `self`.
  // FIXME: Implement derived conformance and remove this default
  // implementation.
  var zeroTangentVector: TangentVector { .zero }
}
// SWIFT_ENABLE_TENSORFLOW END
