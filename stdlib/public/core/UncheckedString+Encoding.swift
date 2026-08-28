//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public enum UncheckedStringFailEncoding {
  case fail
}

public enum UncheckedStringSubstituteEncoding {
  case substitute
}

@available(SwiftStdlib 9999, *)
public extension UncheckedString {
  /// Attempt to decode an `UncheckedString` using the specified encoding
  ///
  /// - Parameters:
  ///   - encoding:  The encoding to use.
  ///   - onInvalidEncoding: `.fail` to return `nil`, or `.substitute` to
  ///                replace invalid encodings with the substitution
  ///                character.
  ///
  /// - Returns a `String` if decoding was successful.
  func decode<Encoding: Unicode.Encoding>(
    as encoding: Encoding.Type,
    onInvalidEncoding: UncheckedStringFailEncoding = .fail
  ) -> String? where Encoding.CodeUnit == Element {
    return withCharacterData { data in
      data.withUnsafeBufferPointer { buffer in
        guard let result = unsafe String._fromCodeUnits(
          buffer,
          encoding: Encoding.self,
          repair: false
        ) else {
          return nil
        }

        return result.0
      }
    }
  }

  /// Attempt to decode an `UncheckedString` using the specified encoding
  ///
  /// - Parameters:
  ///   - encoding:  The encoding to use.
  ///   - onInvalidEncoding: `.fail` to return `nil`, or `.substitute` to
  ///                replace invalid encodings with the substitution
  ///                character.
  ///
  /// - Returns a `String` if decoding was successful.
  func decode<Encoding: Unicode.Encoding>(
    as encoding: Encoding.Type,
    onInvalidEncoding: UncheckedStringSubstituteEncoding
  ) -> String where Encoding.CodeUnit == Element {
    return withCharacterData { data in
      data.withUnsafeBufferPointer { buffer in
        let result = unsafe String._fromCodeUnits(
          buffer,
          encoding: Encoding.self,
          repair: true
        )!

        return result.0
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
public extension String {
  /// Attempt to encode a `String` using the specified encoding.
  ///
  /// - Parameters:
  ///   - encoding:  The encoding to use.
  ///   - onUnsupportedEncoding: `.fail` to return `nil`, or `.substitute`
  ///                to use a substitution character.
  ///
  /// - Returns an `UncheckedString` if encoding was successful.
  func encode<Encoding: Unicode.Encoding>(
    as encoding: Encoding.Type,
    onUnsupportedEncoding: UncheckedStringFailEncoding = .fail
  ) -> UncheckedString<Encoding.CodeUnit>? {
    var data: [Encoding.CodeUnit] = []
    data.reserveCapacity(self.utf8.count)
    let repaired = transcode(
      self.utf8.makeIterator(),
      from: UTF8.self,
      to: Encoding.self,
      stoppingOnError: true,
      into: { data.append($0) })
    if repaired {
      return nil
    }

    return UncheckedString(taking: data)
  }

  /// Attempt to encode a `String` using the specified encoding.
  ///
  /// - Parameters:
  ///   - encoding:  The encoding to use.
  ///   - onUnsupportedEncoding: `.fail` to return `nil`, or `.substitute`
  ///                to use a substitution character.
  ///
  /// - Returns an `UncheckedString` if encoding was successful.
  func encode<Encoding: Unicode.Encoding>(
    as encoding: Encoding.Type,
    onUnsupportedEncoding: UncheckedStringSubstituteEncoding
  ) -> UncheckedString<Encoding.CodeUnit> {
    var data: [Encoding.CodeUnit] = []
    data.reserveCapacity(self.utf8.count)
    _ = transcode(
      self.utf8.makeIterator(),
      from: UTF8.self,
      to: Encoding.self,
      stoppingOnError: false,
      into: { data.append($0) })

    return UncheckedString(taking: data)
  }
}