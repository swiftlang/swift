//===--- CrashLog.swift ---------------------------------------------------===//
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

import Foundation

/// A parsed crash log.
public struct CrashLog: Sendable {
  public var frames: [Frame]
  public var isStackOverflow: Bool
  public var signature: Signature

  // TODO: Ideally this ought to be a blocklist, not an allowlist (the goal
  // here is to exclude frames in system libraries).
  static let targetImageNames: Set<String> = [
    "swift-frontend", "swift-ide-test", "lib_CompilerSwiftParser.dylib"
  ]

  static let sanitizerFrameSymbolRegex =
    #/^\s*?SUMMARY: \w+Sanitizer:\s*\w+\s*\([^:]+?(?<image>[\w-]+):[^)]+\)\s*in\s*(?<symbol>.+)$/#

  static let abortRegex = #/^Abort:\s*function\s*(?<symbol>[^\s]+).*$/#

  private static func checkStackOverflow(_ lines: [String]) -> Bool {
    lines.contains { $0.scanningUTF8 { $0.scanForStackOverflow() } }
  }

  private static func getFrames(from lines: [String]) -> [Frame] {
    var lines = lines[...]

    guard let stackDumpStart = lines.firstIndex(where: {
      $0.hasPrefix("Stack dump without symbol names")
    }) ?? lines.firstIndex(where: {
      $0.contains("Stack dump without symbol names")
    }) else {
      // The frame symbol can be included in the UBSan error.
      for line in lines {
        guard let match = line.wholeMatch(of: sanitizerFrameSymbolRegex)?.output else {
          continue
        }
        return [
          Frame(
            line: line,
            image: String(match.image),
            symbol: String(match.symbol),
            offset: nil
          )
        ]
      }
      return []
    }
    lines = lines[(stackDumpStart + 1)...]

    var frames: [Frame] = []
    while let line = lines.first, let frame = Frame(from: line) {
      frames.append(frame)
      lines = lines.dropFirst()
    }
    return frames
  }

  /// Given a set of stack frames, find the deepest one that should be considered
  /// the signature of the crash.
  private static func findFrameSignature(
    _ frames: [Frame], isStackOverflow: Bool
  ) -> String? {
    guard let lastFrameIdx = frames.lastIndex(
      where: { Self.targetImageNames.contains($0.image) })
    else {
      return nil
    }
    let firstFrameIdx = frames[...lastFrameIdx].reversed().firstIndex(where: {
      !Self.targetImageNames.contains($0.image)
    })?.base ?? frames.startIndex

    let unfiltered = Array(frames[firstFrameIdx ... lastFrameIdx])
    let filtered = unfiltered.filter { frame in
      // Ignore frames with 0 offset, they're almost certainly bogus.
      frame.offset != 0 && !Signature.symbol(frame.symbol, assert: nil).shouldIgnore
    }
    let symbols = (filtered.isEmpty ? unfiltered : filtered).map(\.symbol)

    // Check for recursion
    if isStackOverflow,
       let recurse = symbols.reversed().findRepeatedSlice(minRepeats: 10) {
      return recurse.min()
    }
    return symbols.first
  }

  private static func findAbort(_ lines: [String]) -> String? {
    for line in lines {
      if let match = line.wholeMatch(of: Self.abortRegex) {
        return String(match.symbol)
      }
    }
    return nil
  }

  private static func findAssertion(_ lines: [String]) -> Assertion? {
    for line in lines {
      guard let assert = Assertion(from: line) else { continue }
      return assert
    }
    return nil
  }

  public init?(from log: String) {
    let lines = log.components(separatedBy: "\n")
    self.isStackOverflow = Self.checkStackOverflow(lines)
    self.frames = Self.getFrames(from: lines)

    let assertion = Self.findAssertion(lines)

    var sig: Signature?

    sig = Self.findFrameSignature(frames, isStackOverflow: isStackOverflow)
      .map { Signature(symbol: $0, assertion: assertion) }

    if sig == nil {
      sig = Self.findAbort(lines)
        .map({ Signature(symbol: $0, assertion: assertion) })
    }
    if sig == nil {
      sig = assertion.map(Signature.assertion)
    }
    guard let sig = sig else { return nil }
    self.signature = sig
  }
}

extension CrashLog {
  public struct Frame: Sendable {
    public var line: String
    public var image: String
    public var symbol: String
    public var offset: Int?

    public init(line: String, image: String, symbol: String, offset: Int?) {
      self.line = line
      self.image = image
      self.symbol = symbol.droppingFrameTrailingJunk
        .trimmingCharacters(in: .whitespaces)
      self.offset = offset
    }
  }
}

extension ByteScanner {
  fileprivate mutating func scanForStackOverflow() -> Bool {
    // #/^\s*?SUMMARY:\s*\w+Sanitizer:\s*stack-overflow.*$/#

    skip(while: \.isSpaceOrTab)

    guard tryEat(utf8: "SUMMARY:") else { return false }

    skip(while: \.isSpaceOrTab)

    guard peek?.isWordChar == true else { return false }

    while peek?.isWordChar == true {
      if tryEat(utf8: "Sanitizer:") {
        break
      }
      _ = eat()
    }

    skip(while: \.isSpaceOrTab)

    return tryEat(utf8: "stack-overflow")
  }

  fileprivate mutating func parseCrashFrame() -> CrashLog.Frame? {
    skip(while: \.isSpaceOrTab)

    // Frame index
    let start = cursor
    skip(while: \.isDigit)
    guard start < cursor else { return nil }

    skip(while: \.isSpaceOrTab)

    // Image
    guard let imgBytes = eat(while: { !$0.isSpaceTabOrNewline }) else {
      return nil
    }
    let img = String(utf8: imgBytes)

    skip(while: \.isSpaceOrTab)

    // Addr
    guard eat(while: { !$0.isSpaceTabOrNewline }) != nil else { return nil }

    skip(while: \.isSpaceOrTab)

    // Symbol
    guard var symbolBytes = eat(while: { !$0.isNewline }) else { return nil }
    let symLastNonWhitespace = symbolBytes.lastIndex(where: { !Byte($0).isSpaceOrTab })!
    symbolBytes = .init(rebasing: symbolBytes[...symLastNonWhitespace])

    let line = decodeUTF8(start ..< symbolBytes.endIndex)

    // FIXME: This is a mess
    var offset: Int?
    if let plus = symbolBytes.lastIndex(of: Byte("+").rawValue) {
      let offsetBytes = symbolBytes[(plus + 1)...].drop(while: { Byte($0).isSpaceOrTab })
      offset = Int(String(utf8: offsetBytes))
      symbolBytes = .init(rebasing: symbolBytes[..<plus])
      let symLastNonWhitespace = symbolBytes.lastIndex(where: { !Byte($0).isSpaceOrTab })!
      symbolBytes = .init(rebasing: symbolBytes[...symLastNonWhitespace])
    }
    let symbol = String(utf8: symbolBytes).droppingFrameTrailingJunk
      .trimmingCharacters(in: .whitespaces)

    return CrashLog.Frame(line: line, image: img, symbol: symbol, offset: offset)
  }
}

extension CrashLog.Frame {
  init?(from line: String) {
    guard let match = line.scanningUTF8({ $0.parseCrashFrame() }) else {
      return nil
    }
    self = match
  }
}

fileprivate extension String {
  var droppingFrameTrailingJunk: String {
    let rev = Array(utf8.reversed())
    return rev.withUnsafeBufferPointer { buffer in
      var scanner = ByteScanner(buffer)
      scanner.skip(while: \.isSpaceOrTab)
      if scanner.peek?.isHexDigit == true {
        var tmp = scanner
        tmp.skip(while: { $0.isHexDigit || $0 == "x" })
        tmp.skip(while: \.isSpaceOrTab)
        if tmp.tryEat("+") {
          scanner = tmp
          scanner.skip(while: \.isSpaceOrTab)
        }
      }
      var tmp = scanner
      if tmp.tryEat(")"), tmp.peek?.isDigit == true {
        tmp.skip(while: \.isDigit)
        if tmp.tryEat("."), tmp.peek?.isWordChar == true {
          tmp.skip(while: \.isWordChar)
          if tmp.tryEat("."), tmp.tryEat("(") {
            scanner = tmp
          }
        }
      }
      return scanner.remaining.reversed().withUnsafeBufferPointer {
        String(utf8: $0)
      }
    }
  }
}

extension ByteScanner {
  fileprivate mutating func parseIgnorableCollectionType() -> Bool {
    while peek?.isWordChar == true {
      if tryEat(utf8: "Dense"), tryEat(utf8: "Map") || tryEat(utf8: "Set") {
        skip(untilAfter: \.isWordChar)
        return true
      }
      if tryEat(utf8: "vector", caseSensitive: false) {
        return true
      }
      _ = eat()
    }
    return false
  }
}

extension String {
  fileprivate var shouldIgnoreShortSymbol: Bool {
    // Ignore inline collection frames.
    if scanningUTF8({
      $0.parseIgnorableCollectionType() && $0.tryEat(utf8: "::")
    }) {
      return true
    }
    return false
  }
}

fileprivate extension Signature {
  var shouldIgnore: Bool {
    // Ignore llvm/assert frames.
    if let symbol {
      let longIgnore = symbol.scanningUTF8 { scanner in
        if scanner.tryEat(utf8: "llvm::") ||
            scanner.tryEat(utf8: "std::") ||
            scanner.tryEat(utf8: "swift::ASTVisitor<") {
          return true
        }
        while scanner.hasInput {
          if scanner.tryEat(utf8: "assert", caseSensitive: false) {
            return true
          }
          if scanner.tryEat(utf8: "abort", caseSensitive: false) {
            return true
          }
          _ = scanner.eat()
        }
        return false
      }
      if longIgnore {
        return true
      }
    }
    if let short, short.namespace == "llvm" || short.namespace == "std" ||
       short.symbol.shouldIgnoreShortSymbol {
      return true
    }
    return false
  }
}
