//===--- Code.swift -------------------------------------------------------===//
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

public import Foundation

/// A wrapper for a string of Swift source code.
public struct Code: Sendable, Hashable {
  public var text: String

  public init(_ text: String) {
    self.text = text
  }
  public init(_ text: Substring) {
    self.text = String(text)
  }
  public init(decoding data: Data) throws {
    self.init(String(decoding: data, as: UTF8.self))
  }
  public init(from path: AbsolutePath) throws {
    try self.init(decoding: path.read())
  }
}

extension Code {
  private static let commentRegex = #///.*$/#
  private static let repeatedWhitespaceRegex = #/\s\s+/#
  private static let leadingWhitespaceRegex = #/^\s+/#
  private static let trailingWhitespaceRegex = #/\s+$/#
  private static let importRegex = #/^\s*import\s+\w+/#
  private static let objCRegex = #/@objc|#keyPath/#

  private static func cleanupLine(
    _ line: String, includingWhitespace: Bool
  ) -> String? {
    var line = line
    // Remove comments, ensuring we don't get interference from RUN or CHECK
    // directives. We unfortunately can't use swift-syntax, since we could
    // have a syntax crasher.
    if let match = line.firstMatch(of: Self.commentRegex) {
      if line[..<match.range.lowerBound].last != "#" { // #// is regex
        line.removeSubrange(match.range)
      }
    }
    if line.allSatisfy(\.isWhitespace) {
      return nil
    }
    if let match = line.firstMatch(of: Self.trailingWhitespaceRegex) {
      line.removeSubrange(match.range)
    }
    if includingWhitespace {
      if let match = line.firstMatch(of: Self.leadingWhitespaceRegex) {
        line.removeSubrange(match.range)
      }
      // Trim unnecessary repeated whitespace.
      if let firstChar = line.firstIndex(where: { !$0.isWhitespace }) {
        line.replaceSubrange(
          firstChar...,
          with: line[firstChar...].replacing(
            Self.repeatedWhitespaceRegex, with: " "
          )
        )
      }
    }
    return line
  }

  func cleanupTrivia(includingWhitespace: Bool) -> Self {
    let text = text.components(separatedBy: "\n")
      .compactMap { Self.cleanupLine($0, includingWhitespace: includingWhitespace) }
      .joined(separator: "\n")
      .trimmingCharacters(in: .whitespacesAndNewlines)
    return Self(text)
  }

  var hasImport: Bool {
    text.firstMatch(of: Self.importRegex) != nil
  }
  var hasObjC: Bool {
    text.firstMatch(of: Self.objCRegex) != nil
  }

  func split(_ splits: [Int]) throws -> [Code] {
    let utf8 = text.utf8
    let splitOffsets = [0] + splits + [utf8.count]
    var buffers: [Code] = []
    for (lower, upper) in zip(splitOffsets, splitOffsets.dropFirst()) {
      // Would be quadratic if splits were a factor of length, but that's
      // currently not the case.
      guard let start = utf8.index(utf8.startIndex, offsetBy: lower, limitedBy: utf8.endIndex) else {
        throw ReproducerError("\(lower) exceeds buffer bounds \(utf8.count)")
      }
      guard let end = utf8.index(utf8.startIndex, offsetBy: upper, limitedBy: utf8.endIndex) else {
        throw ReproducerError("\(upper) exceeds buffer bounds \(utf8.count)")
      }
      buffers.append(Code(String(text[start ..< end])))
    }
    return buffers
  }
}
