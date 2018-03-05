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
// This file defines the DifferentiationArgument type, used by gradient
// expressions and the differentiable attribute.
//
//===----------------------------------------------------------------------===//

/// A type that represents a valid argument for automatic differentiation.
///
/// Types that conform to the `DifferentiationArgument` protocol can be
/// differentiated with-respect-to in #gradient and #valueAndGradient
/// expressions.
///
/// TODO: improve description and add examples.
public protocol DifferentiationArgument {
  /// Returns an instance initialized to zero.
  func makeZero() -> Self
  /// Returns an instance initialized to one.
  func makeOne() -> Self
  /// Adds two values and produces their sum.
  static func + (lhs: Self, rhs: Self) -> Self
}

extension Float : DifferentiationArgument {
  /// Returns a Float initialized to zero.
  public func makeZero() -> Float { return 0 }
  /// Returns a Float initialized to one.
  public func makeOne() -> Float { return 1 }
}

extension Double : DifferentiationArgument {
  /// Returns a Double initialized to zero.
  public func makeZero() -> Double { return 0 }
  /// Returns a Double initialized to one.
  public func makeOne() -> Double { return 1 }
}
