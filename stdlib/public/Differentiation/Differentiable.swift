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
}

public extension Differentiable where TangentVector == Self {
  @_alwaysEmitIntoClient
  mutating func move(along direction: TangentVector) {
    self += direction
  }
}

//===----------------------------------------------------------------------===//
// `Differentiable` conformances
//===----------------------------------------------------------------------===//

extension Float: Differentiable {
  public typealias TangentVector = Self
}
extension Double: Differentiable {
  public typealias TangentVector = Self
}
#if (arch(i386) || arch(x86_64)) && !(os(Windows) || os(Android))
extension Float80: Differentiable {
  public typealias TangentVector = Self
}
#endif
