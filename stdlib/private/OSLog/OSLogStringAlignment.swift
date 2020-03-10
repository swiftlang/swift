//===----------------- OSLogStringAlignment.swift -------------------------===//
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

// This file defines types and functions for specifying alignment of the
// interpolations passed to the os log APIs.

@frozen
public enum OSLogCollectionBound {
  case start
  case end
}

@frozen
public struct OSLogStringAlignment {
  /// Minimum number of characters to be displayed. If the value to be printed
  /// is shorter than this number, the result is padded with spaces. The value
  /// is not truncated even if the result is larger.This value need not be a
  /// compile-time constant, and is therefore an autoclosure.
  public var minimumColumnWidth: (() -> Int)?
  /// This captures right/left alignment.
  public var anchor: OSLogCollectionBound

  /// - Parameters:
  ///   - minimumColumnWidth: Minimum number of characters to be displayed. If the value to be
  ///    printed is shorter than this number, the result is padded with spaces. The value is not truncated
  ///    even if the result is larger.
  ///   - anchor: Use `.end` for right alignment and `.start` for left.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal init(
    minimumColumnWidth: (() -> Int)? = nil,
    anchor: OSLogCollectionBound = .end
  ) {
    self.minimumColumnWidth = minimumColumnWidth
    self.anchor = anchor
  }

  /// Right alignment formatter.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var right: OSLogStringAlignment {
    OSLogStringAlignment(anchor: .end)
  }

  /// Left alignment formatter.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var left: OSLogStringAlignment {
    OSLogStringAlignment(anchor: .start)
  }

  /// Use default alignment, which is right alignment.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var none: OSLogStringAlignment { .right  }

  /// Right align and display at least`columns` characters.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func right(
    columns: @escaping @autoclosure () -> Int
  ) -> OSLogStringAlignment {
    OSLogStringAlignment(minimumColumnWidth: columns, anchor: .end)
  }

  /// Left align and display at least`columns` characters.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func left(
    columns: @escaping @autoclosure () -> Int
  ) -> OSLogStringAlignment {
    OSLogStringAlignment(minimumColumnWidth: columns, anchor: .start)
  }
}
