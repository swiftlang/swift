//===--------------- SwiftLanguage.swift - Swift Syntax Library -----------===//
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
// This file provides main entry point into the Syntax library.
//===----------------------------------------------------------------------===//

import Foundation

#if os(macOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

/// A list of possible errors that could be encountered while parsing a
/// Syntax tree.
public enum ParserError: Error, CustomStringConvertible {
  case swiftcFailed(Int, String)
  case invalidFile

  public var description: String {
    switch self{
    case let .swiftcFailed(exitCode, stderr):
      let stderrLines = stderr.split(separator: "\n")
      return """
      swiftc returned with non-zero exit code \(exitCode)
      stderr:
        \(stderrLines.joined(separator: "\n  "))
      """
    case .invalidFile:
      return "swiftc created an invalid syntax file"
    }
  }
}

extension Syntax {
  fileprivate static func encodeSourceFileSyntaxInternal(_ url: URL) throws -> Data {
    let swiftcRunner = try SwiftcRunner(sourceFile: url)
    let result = try swiftcRunner.invoke()
    guard result.wasSuccessful else {
      throw ParserError.swiftcFailed(result.exitCode, result.stderr)
    }
    return result.stdoutData
  }

  /// Parses the Swift file at the provided URL into a `Syntax` tree in Json
  /// serialization format.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: The syntax tree in Json format string.
  public static func encodeSourceFileSyntax(_ url: URL) throws -> String {
    return String(data: try encodeSourceFileSyntaxInternal(url), encoding: .utf8)!
  }

  /// Parses the Swift file at the provided URL into a full-fidelity `Syntax`
  /// tree.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  /// - Throws: `ParseError.couldNotFindSwiftc` if `swiftc` could not be
  ///           located, `ParseError.invalidFile` if the file is invalid.
  ///           FIXME: Fill this out with all error cases.
  public static func parse(_ url: URL) throws -> SourceFileSyntax {
    return try decodeSourceFileSyntax(encodeSourceFileSyntaxInternal(url))
  }

  /// Decode a serialized form of SourceFileSyntax to a syntax node.
  /// - Parameter content: The data of the serialized SourceFileSyntax.
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  fileprivate static func decodeSourceFileSyntax(_ content: Data) throws -> SourceFileSyntax {
    let decoder = JSONDecoder()
    let raw = try decoder.decode(RawSyntax.self, from: content)
    guard let file = makeSyntax(raw) as? SourceFileSyntax else {
      throw ParserError.invalidFile
    }
    return file
  }

  /// Decode a serialized form of SourceFileSyntax to a syntax node.
  /// - Parameter content: The string content of the serialized SourceFileSyntax.
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  public static func decodeSourceFileSyntax(_ content: String) throws -> SourceFileSyntax {
    return try decodeSourceFileSyntax(content.data(using: .utf8)!)
  }
}
