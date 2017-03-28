//===--- StringInterpolation.swift - String Interpolation -----------------===//
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

extension String : _ExpressibleByStringInterpolation {
  /// Creates a new string by concatenating the given interpolations.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you create a string using string interpolation. Instead, use string
  /// interpolation to create a new string by including values, literals,
  /// variables, or expressions enclosed in parentheses, prefixed by a
  /// backslash (`\(`...`)`).
  ///
  ///     let price = 2
  ///     let number = 3
  ///     let message = "If one cookie costs \(price) dollars, " +
  ///                   "\(number) cookies cost \(price * number) dollars."
  ///     print(message)
  ///     // Prints "If one cookie costs 2 dollars, 3 cookies cost 6 dollars."
  @effects(readonly)
  public init(stringInterpolation segments: StringInterpolationSegment<String, String>...) {
    self.init()
    for segment in segments {
      switch segment {
      case .literal(let str), .interpolation(let str):
        self += str
      }
    }
  }

  public init<T> (stringInterpolationSegment expr: T) {
    self = String(describing: expr)
  }

  /// Creates a string containing the given value's textual representation.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// interpreting string interpolations.
  ///
  /// - SeeAlso: `ExpressibleByStringInterpolation`
  public init<T: TextOutputStreamable> (stringInterpolationSegment expr: T) {
    self = _toStringReadOnlyStreamable(expr)
  }

  /// Creates a string containing the given value's textual representation.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// interpreting string interpolations.
  ///
  /// - SeeAlso: `ExpressibleByStringInterpolation`
  public init<T: CustomStringConvertible> (stringInterpolationSegment expr: T) {
    self = _toStringReadOnlyPrintable(expr)
  }

  /// Creates a string containing the given value's textual representation.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// interpreting string interpolations.
  ///
  /// - SeeAlso: `ExpressibleByStringInterpolation`
  public init<T: TextOutputStreamable & CustomStringConvertible> (stringInterpolationSegment expr: T) {
    self = _toStringReadOnlyStreamable(expr)
  }
}
