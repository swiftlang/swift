//===--- StringCharacterView.swift - String's Collection of Characters ----===//
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
//  String is-not-a Sequence or Collection, but it exposes a
//  collection of characters.
//
//===----------------------------------------------------------------------===//

// FIXME(ABI)#70 : The character string view should have a custom iterator type
// to allow performance optimizations of linear traversals.

import SwiftShims

extension String {
  /// A view of a string's contents as a collection of characters.
  ///
  /// Previous versions of Swift provided this view since String
  /// itself was not a collection. String is now a collection of
  /// characters, so this type is now just an alias for String.
  @available(swift, deprecated: 3.2, obsoleted: 5.0, 
    message: "Please use String directly")
  public typealias CharacterView = String

  /// A view of the string's contents as a collection of characters.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, obsoleted: 5.0, 
    message: "Please use String directly")
  public var characters: String {
    get {
      return self
    }
    set {
      self = newValue
    }
  }

  /// Applies the given closure to a mutable view of the string's characters.
  ///
  /// Previous versions of Swift provided this view since String
  /// itself was not a collection. String is now a collection of
  /// characters, so this type is now just an alias for String.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, obsoleted: 5.0, 
    message: "Please mutate the String directly")
  public mutating func withMutableCharacters<R>(
    _ body: (inout String) -> R
  ) -> R {
    return body(&self)
  }
}

extension Substring {
  /// A view of a string's contents as a collection of characters.
  ///
  /// Previous versions of Swift provided this view since String
  /// itself was not a collection. String is now a collection of
  /// characters, so this type is now just an alias for String.
  @available(swift, deprecated: 3.2, obsoleted: 5.0, 
    message: "Please use Substring directly")
  public typealias CharacterView = Substring

  /// A view of the string's contents as a collection of characters.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, obsoleted: 5.0,
    message: "Please use Substring directly")
  public var characters: Substring {
    get {
      return self
    }
    set {
      self = newValue
    }
  }

  /// Applies the given closure to a mutable view of the string's characters.
  ///
  /// Previous versions of Swift provided this view since String
  /// itself was not a collection. String is now a collection of
  /// characters, so this type is now just an alias for String.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, obsoleted: 5.0, 
    message: "Please mutate the Substring directly")
  public mutating func withMutableCharacters<R>(
    _ body: (inout Substring) -> R
  ) -> R {
    return body(&self)
  }
}
