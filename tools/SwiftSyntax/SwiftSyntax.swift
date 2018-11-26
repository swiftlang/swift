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

/// Deserializes the syntax tree from its serialized form to an object tree in
/// Swift. To deserialize incrementally transferred syntax trees, the same
/// instance of the deserializer must be used for all subsequent
/// deserializations.
public final class SyntaxTreeDeserializer {
  // FIXME: This lookup table just accumulates nodes, we should invalidate nodes
  // that are no longer used at some point and remove them from the table

  /// Syntax nodes that have already been parsed and are able to be reused if
  /// they were omitted in an incremental syntax tree transfer
  private var nodeLookupTable: [SyntaxNodeId: RawSyntax] = [:]

  public init() {
  }

  /// Decode a serialized form of SourceFileSyntax to a syntax tree.
  /// - Parameter data: The UTF-8 represenation of the serialized syntax tree
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  public func deserialize(_ data: Data) throws -> SourceFileSyntax {
    let decoder = JSONDecoder()
    decoder.userInfo[.rawSyntaxDecodedCallback] = self.addToLookupTable
    decoder.userInfo[.omittedNodeLookupFunction] = self.lookupNode
    let raw = try decoder.decode(RawSyntax.self, from: data)
    guard let file = makeSyntax(raw) as? SourceFileSyntax else {
      throw ParserError.invalidFile
    }
    return file
  }

  // MARK: Incremental deserialization helper functions

  private func lookupNode(id: SyntaxNodeId) -> RawSyntax? {
    return nodeLookupTable[id]
  }

  private func addToLookupTable(_ node: RawSyntax) {
    nodeLookupTable[node.id] = node
  }
}

/// Namespace for functions to retrieve a syntax tree from the swift compiler
/// and deserializing it.
public enum SyntaxTreeParser {
  /// Parses the Swift file at the provided URL into a full-fidelity Syntax tree
  /// - Parameter url: The URL you wish to parse.
  /// - Returns: A top-level Syntax node representing the contents of the tree,
  ///            if the parse was successful.
  /// - Throws: `ParseError.couldNotFindSwiftc` if `swiftc` could not be
  ///           located, `ParseError.invalidFile` if the file is invalid.
  ///           FIXME: Fill this out with all error cases.
  public static func parse(_ url: URL) throws -> SourceFileSyntax {
    let swiftcRunner = try SwiftcRunner(sourceFile: url)
    let result = try swiftcRunner.invoke()
    let syntaxTreeData = result.stdoutData
    let deserializer = SyntaxTreeDeserializer()
    return try deserializer.deserialize(syntaxTreeData)
  }
}

