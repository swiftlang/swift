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
extension _Unicode {
  public enum ParseResult<T> {
  case valid(T)
  case emptyInput
  case invalid(length: Int)

    var isEmpty : Bool {
      switch self {
      case .emptyInput: return true
      default: return false
      }
    }
  }
}

/// Types that separate streams of code units into encoded unicode scalar values
public protocol UnicodeParser {
  /// The encoding with which this parser is associated
  associatedtype Encoding : _UnicodeEncoding

  init()

  /// Parses a single Unicode scalar value from `input`.
  mutating func parseScalar<I : IteratorProtocol>(
    from input: inout I
  ) -> _Unicode.ParseResult<Encoding.EncodedScalar>
  where I.Element == Encoding.CodeUnit
}

extension UnicodeParser {
  @inline(__always)
  @discardableResult
  public static func parse<I: IteratorProtocol>(
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
      case .invalid:
        if _slowPath(!makeRepairs) { return 1 }
        errorCount += 1
        output(Encoding.encodedReplacementCharacter)
      case .emptyInput:
        return errorCount
      }
    }
  }

  @inline(__always)
  @discardableResult
  public static func decode<I: IteratorProtocol>(
    _ input: inout I,
    repairingIllFormedSequences makeRepairs: Bool,
    into output: (UnicodeScalar)->Void
  ) -> Int
  where I.Element == Encoding.CodeUnit
  {
    return parse(&input, repairingIllFormedSequences: makeRepairs) {
      output(Encoding.decode($0))
    }
  }
}

extension _Unicode {
  @_fixed_layout
  public struct ParsingIterator<
    CodeUnitIterator : IteratorProtocol, 
    Parser: UnicodeParser
  > where Parser.Encoding.CodeUnit == CodeUnitIterator.Element {
    @inline(__always)
    @_inlineable
    public init(codeUnits: CodeUnitIterator, parser: Parser) {
      self.codeUnits = codeUnits
      self.parser = parser
    }
    public var codeUnits: CodeUnitIterator
    public var parser: Parser
  }
}

extension _Unicode.ParsingIterator : IteratorProtocol, Sequence {
  @inline(__always)
  @_inlineable
  public mutating func next() -> Parser.Encoding.EncodedScalar? {
    switch parser.parseScalar(from: &codeUnits) {
    case let .valid(scalarContent): return scalarContent
    case .invalid: return Parser.Encoding.encodedReplacementCharacter
    case .emptyInput: return nil
    }
  }
}

