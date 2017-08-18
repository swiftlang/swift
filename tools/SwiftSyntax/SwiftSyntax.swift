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

public typealias ToolHandler = (SourceFileSyntax, DiagnosticEngine) -> Int

/// This is the main entry point for Swift tools that would like to
/// emit diagnostics for Swift.
/// Calling this function will pass your command-line arguments to swiftc, which
/// will then pass you a SourceFileSyntax in the handler you provide. Along with
/// that, you'll receive a diagnostic engine that you can use to provide swiftc
/// with custom diagnostics.
/// - Parameter body: The main body of your tool. Its return value must be
///                   the exit status code you intend your tool to finish with.
/// - Note: This function calls exit(_:) on your behalf.
public func runSwiftTool(_ body: ToolHandler) throws -> Never {
  let fileURL = URL(fileURLWithPath: "/tmp/foo.swift")
  let outputURL = fileURL.deletingPathExtension()
                         .appendingPathExtension("dia")
  let consumer = SerializedDiagnosticConsumer(outputURL: outputURL)
  let printingConsumer = PrintingDiagnosticConsumer()

  let engine = DiagnosticEngine()
  engine.addConsumer(consumer)
  engine.addConsumer(printingConsumer)

  let sourceFile = try Syntax.parse(fileURL)
  let exitCode = body(sourceFile, engine)

  engine.finalize()

  exit(Int32(exitCode))
}
