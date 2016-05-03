//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension String.Index {
  /// Construct the position in `characters` that corresponds exactly to
  /// `unicodeScalarIndex`. If no such position exists, the result is `nil`.
  ///
  /// - Precondition: `unicodeScalarIndex` is an element of
  ///   `characters.unicodeScalars.indices`.
  public init?(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within characters: String
  ) {
    if !unicodeScalarIndex._isOnGraphemeClusterBoundary {
      return nil
    }
    self.init(_base: unicodeScalarIndex)
  }

  /// Construct the position in `characters` that corresponds exactly to
  /// `utf16Index`. If no such position exists, the result is `nil`.
  ///
  /// - Precondition: `utf16Index` is an element of
  ///   `characters.utf16.indices`.
  public init?(
    _ utf16Index: String.UTF16Index,
    within characters: String
  ) {
    if let me = utf16Index.samePosition(
      in: characters.unicodeScalars
    )?.samePosition(in: characters) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Construct the position in `characters` that corresponds exactly to
  /// `utf8Index`. If no such position exists, the result is `nil`.
  ///
  /// - Precondition: `utf8Index` is an element of
  ///   `characters.utf8.indices`.
  public init?(
    _ utf8Index: String.UTF8Index,
    within characters: String
  ) {
    if let me = utf8Index.samePosition(
      in: characters.unicodeScalars
    )?.samePosition(in: characters) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Returns the position in `utf8` that corresponds exactly
  /// to `self`.
  ///
  /// - Precondition: `self` is an element of `String(utf8).indices`.
  @warn_unused_result
  public func samePosition(
    in utf8: String.UTF8View
  ) -> String.UTF8View.Index {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Returns the position in `utf16` that corresponds exactly
  /// to `self`.
  ///
  /// - Precondition: `self` is an element of `String(utf16).indices`.
  @warn_unused_result
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Returns the position in `unicodeScalars` that corresponds exactly
  /// to `self`.
  ///
  /// - Precondition: `self` is an element of `String(unicodeScalars).indices`.
  @warn_unused_result
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarView.Index {
    return String.UnicodeScalarView.Index(self, within: unicodeScalars)
  }
}

