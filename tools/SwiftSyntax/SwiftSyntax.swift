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

/// A list of possible errors that could be encountered while parsing a
/// Syntax tree.
public enum ParserError: Error {
  case swiftcFailed(Int, String)
  case invalidFile
}

extension Syntax {
  /// Parses the Swift file at the provided URL into a full-fidelity `Syntax`
  /// tree.
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  /// - Throws: `ParseError.couldNotFindSwiftc` if `swiftc` could not be
  ///           located, `ParseError.invalidFile` if the file is invalid.
  ///           FIXME: Fill this out with all error cases.
  public static func parse(_ url: URL) throws -> SourceFileSyntax {
    let swiftcRunner = try SwiftcRunner(sourceFile: url)
    let result = try swiftcRunner.invoke()
    guard result.wasSuccessful else {
      throw ParserError.swiftcFailed(result.exitCode, result.stderr)
    }
    let decoder = JSONDecoder()
    let raw = try decoder.decode(RawSyntax.self, from: result.stdoutData)
    guard let file = Syntax.fromRaw(raw) as? SourceFileSyntax else {
      throw ParserError.invalidFile
    }
    return file
  }
}
