//===----------------- OSLogSwiftProtocols.swift -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file defines extensions for interpolating types conforming to common
// Swift protocols. It defines `appendInterpolation` overloads for these protocols.
// All overloads defined in this file, delegate to other appendInterpolation
// functions for types natively supported by os_log.

extension OSLogInterpolation {

  /// Defines interpolation for values conforming to CustomStringConvertible. The values
  /// are displayed using the description methods on them.
  ///
  /// Do not call this function directly. It will be called automatically when interpolating
  /// a value conforming to CustomStringConvertible in the string interpolations passed
  /// to the log APIs.
  ///
  /// - Parameters:
  ///   - value: The interpolated expression conforming to CustomStringConvertible.
  ///   - align: Left or right alignment with the minimum number of columns as
  ///     defined by the type `OSLogStringAlignment`.
  ///   - privacy: A privacy qualifier which is either private or public.
  ///     It is auto-inferred by default.
  @_optimize(none)
  @_transparent
  @_semantics("oslog.requires_constant_arguments")
  public mutating func appendInterpolation<T : CustomStringConvertible>(
    _ value: @autoclosure @escaping () -> T,
    align: OSLogStringAlignment = .none,
    privacy: OSLogPrivacy = .auto
  ) {
    // TODO: Dead code elimination does not remove the call to the default value
    // of alignment: .none. This function is made @_transparent to work around
    // that.
    appendInterpolation(value().description, align: align, privacy: privacy)
  }

  /// Defines interpolation for meta-types.
  ///
  /// Do not call this function directly. It will be called automatically when interpolating
  /// a value of type `Any.Type` in the string interpolations passed to the log APIs.
  ///
  /// - Parameters:
  ///   - value: An interpolated expression of type Any.Type, which is autoclosured.
  ///   - align: Left or right alignment with the minimum number of columns as
  ///     defined by the type `OSLogStringAlignment`.
  ///   - privacy: A privacy qualifier which is either private or public.
  ///     It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @_semantics("oslog.requires_constant_arguments")
  public mutating func appendInterpolation(
    _ value: @autoclosure @escaping () -> Any.Type,
    align: OSLogStringAlignment = .none,
    privacy: OSLogPrivacy = .auto
  ) {
    appendInterpolation(
      _typeName(value(), qualified: false),
      align: align,
      privacy: privacy)
  }
}
