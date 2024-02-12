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

    /// The lexical context for this expansion.
    let lexicalContext: [Syntax]

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

    init(
      sourceManager: SourceManager,
      lexicalContext: [Syntax],
      discriminator: String
    ) {
      self.sourceManager = sourceManager
      self.lexicalContext = lexicalContext
      self.discriminator = discriminator
    }
  }

  /// Create a new macro expansion context
  func createMacroExpansionContext(
    lexicalContext: [Syntax],
    discriminator: String = ""
  ) -> MacroExpansionContext {
    return MacroExpansionContext(
      sourceManager: self,
      lexicalContext: lexicalContext,
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

#if RESILIENT_SWIFT_SYNTAX
    @unknown default:
      fatalError()
#endif
    }

    let offsetWithinSyntaxNode =
      rawPosition.utf8Offset - node.position.utf8Offset

    var location = exportedSourceFile.sourceLocationConverter.location(
      for: rootPosition.advanced(by: offsetWithinSyntaxNode)
    )

    switch filePathMode {
    case .fileID:
      // The `SourceLocationConverter` in `exportedSourceFile` uses `filePath` as the file mode. When the `fileID` mode
      // is requested, we need to adjust the file and presumed file to the `fileID`.
      let fileID = "\(exportedSourceFile.moduleName)/\(exportedSourceFile.fileName.basename)"
      var adjustedFile = location.file
      if adjustedFile == exportedSourceFile.fileName {
        adjustedFile = fileID
      }
      var adjustedPresumedFile = location.presumedFile
      if adjustedPresumedFile == exportedSourceFile.fileName {
        adjustedPresumedFile = fileID
      }
      location = SourceLocation(
        line: location.line,
        column: location.column,
        offset: location.offset,
        file: adjustedFile,
        presumedLine: location.presumedLine,
        presumedFile: adjustedPresumedFile
      )

    case .filePath:
      break

#if RESILIENT_SWIFT_SYNTAX
    @unknown default:
      fatalError()
#endif
    }

    // Do the location lookup.
    return AbstractSourceLocation(location)
  }
}
