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

extension _UnicodeParser {
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  @discardableResult
  internal static func _parse<I: IteratorProtocol>(
    _ input: inout I,
    repairingIllFormedSequences makeRepairs: Bool = true,
    into output: (Encoding.EncodedScalar)->Void
  ) -> Int
  where I.Element == Encoding.CodeUnit
  {
    var errorCount = 0
    var d = Self()
    while true {
      switch d.parseScalar(from: &input) {
      case let .valid(scalarContent):
        output(scalarContent)
      case .error:
        if _slowPath(!makeRepairs) { return 1 }
        errorCount += 1
        output(Encoding.encodedReplacementCharacter)
      case .emptyInput:
        return errorCount
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  @discardableResult
  public static func _decode<I: IteratorProtocol>(
    _ input: inout I,
    repairingIllFormedSequences makeRepairs: Bool,
    into output: (Unicode.Scalar)->Void
  ) -> Int
  where I.Element == Encoding.CodeUnit
  {
    return _parse(&input, repairingIllFormedSequences: makeRepairs) {
      output(Encoding.decode($0))
    }
  }
}

extension Unicode {
  public typealias Parser = _UnicodeParser
}

extension Unicode {
  @_fixed_layout
  public // @testable
  struct _ParsingIterator<
    CodeUnitIterator : IteratorProtocol, 
    Parser: Unicode.Parser
  > where Parser.Encoding.CodeUnit == CodeUnitIterator.Element {
    @inline(__always)
    @inlinable
    public init(codeUnits: CodeUnitIterator, parser: Parser) {
      self.codeUnits = codeUnits
      self.parser = parser
    }
    public var codeUnits: CodeUnitIterator
    public var parser: Parser
  }
}

extension Unicode._ParsingIterator : IteratorProtocol, Sequence {
  @inline(__always)
  @inlinable
  public mutating func next() -> Parser.Encoding.EncodedScalar? {
    switch parser.parseScalar(from: &codeUnits) {
    case let .valid(scalarContent): return scalarContent
    case .error: return Parser.Encoding.encodedReplacementCharacter
    case .emptyInput: return nil
    }
  }
}

/*
extension Unicode {
  @_fixed_layout
  @usableFromInline
  internal struct _TranscodingIterator<
    SourceCodeUnits : IteratorProtocol,
    Parser : Unicode.Parser,
    TargetEncoding : Unicode.Encoding
  > where Parser.Encoding.CodeUnit == SourceCodeUnits.Element {
    
    @inline(__always)
    @inlinable
    public init(source: SourceCodeUnits, parser: Parser) {
      _scalars = _ParsingIterator(codeUnits: source, parser: parser)
      let firstScalar_ = _scalars.next()
      if _fastPath(firstScalar_ != nil), let firstScalar = firstScalar_ {
        _codeUnits = TargetEncoding._transcode(
          firstScalar, from: Parser.Encoding.self).makeIterator()
      }
      else {
        _codeUnits = TargetEncoding._encode(
          UnicodeScalar(_unchecked: 0)).makeIterator()
        while _codeUnits.next() != nil {  }
        return
      }
    }

    internal var _scalars: _ParsingIterator<SourceCodeUnits, Parser>
    internal var _codeUnits: TargetEncoding.EncodedScalar.Iterator
  }
}


extension Unicode._TranscodingIterator : IteratorProtocol, Sequence {
  @inline(__always)
  @inlinable
  mutating public func next() -> TargetEncoding.CodeUnit? {
    if let x = _codeUnits.next() { return x }
    let nextScalar_ = _scalars.next()
    if _fastPath(nextScalar_ != nil), let nextScalar = nextScalar_ {
      _codeUnits = TargetEncoding._transcode(
        nextScalar, from: Parser.Encoding.self).makeIterator()
    }
    return _codeUnits.next()
  }
}
*/
