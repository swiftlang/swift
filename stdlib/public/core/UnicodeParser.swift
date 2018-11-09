//===--- UnicodeParser.swift ----------------------------------------------===//
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
extension Unicode {
  /// The result of attempting to parse a `T` from some input.
  @_frozen // FIXME(sil-serialize-all)
  public enum ParseResult<T> {
  /// A `T` was parsed successfully
  case valid(T)
  
  /// The input was entirely consumed.
  case emptyInput
  
  /// An encoding error was detected.
  ///
  /// `length` is the number of underlying code units consumed by this
  /// error (the length of the longest prefix of a valid encoding
  /// sequence that could be recognized).
  case error(length: Int)

    @inlinable // FIXME(sil-serialize-all)
    internal var _valid: T? {
      if case .valid(let result) = self { return result }
      return nil
    }

    @inlinable // FIXME(sil-serialize-all)
    internal var _error: Int? {
      if case .error(let result) = self { return result }
      return nil
    }
  }
}

/// Types that separate streams of code units into encoded Unicode
/// scalar values.
public protocol _UnicodeParser {
  /// The encoding with which this parser is associated
  associatedtype Encoding : _UnicodeEncoding

  /// Constructs an instance that can be used to begin parsing `CodeUnit`s at
  /// any Unicode scalar boundary.
  init()

  /// Parses a single Unicode scalar value from `input`.
  mutating func parseScalar<I : IteratorProtocol>(
    from input: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit
}

extension Unicode {
  public typealias Parser = _UnicodeParser
}
