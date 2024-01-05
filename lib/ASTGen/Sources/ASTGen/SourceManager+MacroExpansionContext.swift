//===--- SourceManager+MacroExpansionContext.swift ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxMacros

extension SourceManager {
  class MacroExpansionContext {
    /// The source manager.
    private let sourceManager: SourceManager

    /// The set of diagnostics that were emitted as part of expanding the
    /// macro.
    var diagnostics: [Diagnostic] = []

    /// The macro expansion discriminator, which is used to form unique names
    /// when requested.
    ///
    /// The expansion discriminator is combined with the `uniqueNames` counters
    /// to produce unique names.
    private var discriminator: String

    /// Counter for each of the uniqued names.
    ///
    /// Used in conjunction with `expansionDiscriminator`.
    private var uniqueNames: [String: Int] = [:]

    init(sourceManager: SourceManager, discriminator: String) {
      self.sourceManager = sourceManager
      self.discriminator = discriminator
    }
  }

  /// Create a new macro expansion context
  func createMacroExpansionContext(
    discriminator: String = ""
  ) -> MacroExpansionContext {
    return MacroExpansionContext(
      sourceManager: self,
      discriminator: discriminator
    )
  }
}

extension String {
  /// Retrieve the base name of a string that represents a path, removing the
  /// directory.
  var basename: String {
    guard
      let lastSlash = lastIndex(where: {
        #if os(iOS) || os(macOS) || os(tvOS) || os(watchOS) || os(Android) || os(Linux)
        ["/"].contains($0)
        #else
        ["/", "\\"].contains($0)
        #endif
      })
    else {
      return self
    }

    return String(self[index(after: lastSlash)...])
  }
}

extension SourceManager.MacroExpansionContext: MacroExpansionContext {
  /// Generate a unique name for use in the macro.
  public func makeUniqueName(_ providedName: String) -> TokenSyntax {
    // If provided with an empty name, substitute in something.
    let name = providedName.isEmpty ? "__local" : providedName

    // Grab a unique index value for this name.
    let uniqueIndex = uniqueNames[name, default: 0]
    uniqueNames[name] = uniqueIndex + 1

    // Start with the discriminator.
    var resultString = discriminator

    // Mangle the name
    resultString += "\(name.count)\(name)"

    // Mangle the operator for unique macro names.
    resultString += "fMu"

    // Mangle the index.
    if uniqueIndex > 0 {
      resultString += "\(uniqueIndex - 1)"
    }
    resultString += "_"

    return TokenSyntax(.identifier(resultString), presence: .present)
  }

  /// Produce a diagnostic while expanding the macro.
  public func diagnose(_ diagnostic: Diagnostic) {
    diagnostics.append(diagnostic)
  }

  public func location<Node: SyntaxProtocol>(
    of node: Node,
    at position: PositionInSyntaxNode,
    filePathMode: SourceLocationFilePathMode
  ) -> AbstractSourceLocation? {
    guard let (sourceFile, rootPosition) = sourceManager.rootSourceFile(of: node),
      let exportedSourceFile =
        sourceManager.exportedSourceFilesBySyntax[sourceFile]?.pointee
    else {
      return nil
    }

    // Determine the filename to use in the resulting location.
    let fileName: String
    switch filePathMode {
    case .fileID:
      fileName = "\(exportedSourceFile.moduleName)/\(exportedSourceFile.fileName.basename)"

    case .filePath:
      fileName = exportedSourceFile.fileName
    }

    // Find the node's offset relative to its root.
    let rawPosition: AbsolutePosition
    switch position {
    case .beforeLeadingTrivia:
      rawPosition = node.position

    case .afterLeadingTrivia:
      rawPosition = node.positionAfterSkippingLeadingTrivia

    case .beforeTrailingTrivia:
      rawPosition = node.endPositionBeforeTrailingTrivia

    case .afterTrailingTrivia:
      rawPosition = node.endPosition
    }

    let offsetWithinSyntaxNode =
      rawPosition.utf8Offset - node.position.utf8Offset

    // Do the location lookup.
    let converter = SourceLocationConverter(file: fileName, tree: sourceFile)
    return AbstractSourceLocation(converter.location(for: rootPosition.advanced(by: offsetWithinSyntaxNode)))
  }
}
