//===--- AutoDiff.swift ---------------------------------------*- swift -*-===//
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
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Compiler Protocols
//===----------------------------------------------------------------------===//

/// A type that represents a valid argument for automatic differentiation.
///
/// Types that conform to the `Differentiable` protocol can be
/// differentiated with-respect-to in `#gradient` and `#valueAndGradient`
/// expressions.
///
/// Example:
///
///     struct Vector<Scalar> {
///         var elements: [Scalar]
///         init(_ elements: [Scalar]) { ... }
///     }
///
///     extension Vector: Numeric where Scalar: Numeric { ... }
///
///     extension Vector: Differentiable where Scalar: FloatingPoint {
///         associatedtype DifferentiationCurrency = Scalar
///
///         init(_ value: DifferentiationCurrency, isomorphicTo other: Self) {
///             self.init(Array(repeating: value, count: elements.count))
///         }
///     }
///
public protocol Differentiable {
  /// The currency type in the mathematical model of differentiation. For
  /// example, the currency type of `Float` is `Float`, and the currency type
  /// for a vector of `Float` is still `Float`. The currency type is used to
  /// initialize intermediate values during automatic differentiation, such as
  /// the initial adjoint/tangent and the seed.
  associatedtype DifferentiationCurrency : FloatingPoint

  /// Creates an instance from the specified currency value and another,
  /// structurally isomorphic instance.
  ///
  /// - Parameters:
  ///   - value: The differentiation currency value for initializing the
  ///     instance.
  ///   - other: The other, structurally isomorphic instance.
  ///
  init(_ value: DifferentiationCurrency, isomorphicTo other: Self)

  /// Adds two values and produces their sum.
  static func + (lhs: Self, rhs: Self) -> Self
}
