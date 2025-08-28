//===--- SourceManager.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftOperators
import SwiftSyntax
import SwiftDiagnostics
import swiftASTGen

/// A source manager that keeps track of the source files in the program.
class SourceManager {
  init(cxxDiagnosticEngine: UnsafeMutableRawPointer) {
    self.bridgedDiagEngine = BridgedDiagnosticEngine(raw: cxxDiagnosticEngine)
  }

  /// The bridged diagnostic engine (just the wrapped C++ `DiagnosticEngine`).
  let bridgedDiagEngine: BridgedDiagnosticEngine

  /// The set of source files that have been exported to the C++ code of
  /// the program.
  var exportedSourceFilesBySyntax: [Syntax: UnsafePointer<ExportedSourceFile>] = [:]

  /// The set of nodes that have been detached from their parent nodes.
  ///
  /// The keys are the detached nodes, while the values are (parent node,
  /// offset of detached node while it was attached).
  private var detachedNodes: [Syntax: (Syntax, Int)] = [:]
}

/// MARK: Source file management

extension SourceManager {
  /// Inserts a new source file into the source manager.
  ///
  /// - Returns: `true` if the source file was inserted, `false` if it was
  ///   already there.
  @discardableResult
  func insert(_ sourceFile: UnsafePointer<ExportedSourceFile>) -> Bool {
    let syntax = Syntax(sourceFile.pointee.syntax)
    if exportedSourceFilesBySyntax[syntax] != nil {
      return false
    }

    exportedSourceFilesBySyntax[syntax] = sourceFile
    return true
  }
}

/// MARK: Syntax source location mapping
extension SourceManager {
  /// Detach a given node from its parent, keeping track of where it
  /// occurred in the program.
  func detach<Node: SyntaxProtocol>(
    _ node: Node,
    foldingWith operatorTable: OperatorTable? = nil
  ) -> Node {
    // Already detached
    if node.parent == nil { return node }

    let detached: Node
    if let operatorTable = operatorTable {
      detached = operatorTable.foldAll(node) { _ in }.as(Node.self)!.detached
    } else {
      detached = node.detached
    }

    detachedNodes[Syntax(detached)] = (node.root, node.position.utf8Offset)
    return detached
  }

  /// Find the root source file and offset from within that file for the given
  /// syntax node.
  func rootSyntax<Node: SyntaxProtocol>(
    of node: Node
  ) -> (Syntax, AbsolutePosition) {
    var root = node.root
    var offset = node.position

    // If the root isn't a detached node we know about, there's nothing we
    // can do.
    while let (parent, parentOffset) = detachedNodes[root] {
      root = parent.root
      offset += SourceLength(utf8Length: parentOffset)
    }

    return (root, offset)
  }

  /// Produce the C++ source location for a given position based on a
  /// syntax node.
  func bridgedSourceLoc<Node: SyntaxProtocol>(
    for node: Node,
    at position: AbsolutePosition? = nil
  ) -> SourceLoc {
    // Find the source file and this node's position within it.
    let (rootNode, rootPosition) = rootSyntax(of: node)

    // Find the corresponding exported source file.
    guard let exportedSourceFile = exportedSourceFilesBySyntax[rootNode] else {
      return nil
    }

    // Find the offset of the given position based on the root of the given
    // node.
    let position = position ?? node.position
    let nodeOffset = SourceLength(utf8Length: position.utf8Offset - node.position.utf8Offset)
    let realPosition = rootPosition + nodeOffset

    return SourceLoc(at: realPosition, in: exportedSourceFile.pointee.buffer)
  }
}

extension SourceManager {
  private func diagnoseSingle<Node: SyntaxProtocol>(
    message: String,
    severity: DiagnosticSeverity,
    node: Node,
    position: AbsolutePosition,
    highlights: [Syntax] = [],
    fixItChanges: [FixIt.Change] = []
  ) {
    // Map severity
    let bridgedSeverity: swift.DiagnosticKind = severity.bridged

    // Emit the diagnostic
    var mutableMessage = message
    let diag = mutableMessage.withBridgedString { bridgedMessage in
      BridgedDiagnostic(
        at: bridgedSourceLoc(for: node, at: position),
        message: bridgedMessage,
        severity: bridgedSeverity,
        engine: bridgedDiagEngine
      )
    }

    // Emit highlights
    for highlight in highlights {
      diag.highlight(
        start: bridgedSourceLoc(for: highlight, at: highlight.positionAfterSkippingLeadingTrivia),
        end: bridgedSourceLoc(for: highlight, at: highlight.endPositionBeforeTrailingTrivia)
      )
    }

    // Emit changes for a Fix-It.
    for change in fixItChanges {
      let replaceStartLoc: SourceLoc
      let replaceEndLoc: SourceLoc
      var newText: String

      switch change {
      case .replace(let oldNode, let newNode):
        // Replace the whole node including leading/trailing trivia, but if
        // the trivia are the same, don't include them in the replacing range.
        let leadingMatch = oldNode.leadingTrivia == newNode.leadingTrivia
        let trailingMatch = oldNode.trailingTrivia == newNode.trailingTrivia
        replaceStartLoc = bridgedSourceLoc(
          for: oldNode,
          at: leadingMatch ? oldNode.positionAfterSkippingLeadingTrivia : oldNode.position
        )
        replaceEndLoc = bridgedSourceLoc(
          for: oldNode,
          at: trailingMatch ? oldNode.endPositionBeforeTrailingTrivia : oldNode.endPosition
        )
        var newNode = newNode.detached
        if leadingMatch {
          newNode.leadingTrivia = []
        }
        if trailingMatch {
          newNode.trailingTrivia = []
        }
        newText = newNode.description

      case .replaceLeadingTrivia(let oldToken, let newTrivia):
        guard oldToken.leadingTrivia != newTrivia else {
          continue
        }
        replaceStartLoc = bridgedSourceLoc(for: oldToken)
        replaceEndLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.positionAfterSkippingLeadingTrivia
        )
        newText = newTrivia.description

      case .replaceTrailingTrivia(let oldToken, let newTrivia):
        guard oldToken.trailingTrivia != newTrivia else {
          continue
        }
        replaceStartLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.endPositionBeforeTrailingTrivia
        )
        replaceEndLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.endPosition
        )
        newText = newTrivia.description

      case .replaceChild(let replacingChildData):
        let replacementRange = replacingChildData.replacementRange
        replaceStartLoc = bridgedSourceLoc(
          for: replacingChildData.parent,
          at: replacementRange.lowerBound
        )
        replaceEndLoc = bridgedSourceLoc(
          for: replacingChildData.parent,
          at: replacementRange.upperBound
        )
        newText = replacingChildData.newChild.description

      case .replaceText(
        range: let replacementRange,
        with: let replacementText,
        in: let syntax
      ):
        replaceStartLoc = bridgedSourceLoc(
          for: syntax,
          at: replacementRange.lowerBound
        )
        replaceEndLoc = bridgedSourceLoc(
          for: syntax,
          at: replacementRange.upperBound
        )
        newText = replacementText
      }

      newText.withBridgedString { bridgedMessage in
        diag.fixItReplace(
          start: replaceStartLoc,
          end: replaceEndLoc,
          replacement: bridgedMessage
        )
      }
    }

    diag.finish();
  }

  /// Emit a diagnostic via the C++ diagnostic engine.
  func diagnose(
    diagnostic: Diagnostic,
    messageSuffix: String? = nil
  ) {
    // Emit the main diagnostic.
    diagnoseSingle(
      message: diagnostic.diagMessage.message + (messageSuffix ?? ""),
      severity: diagnostic.diagMessage.severity,
      node: diagnostic.node,
      position: diagnostic.position,
      highlights: diagnostic.highlights
    )

    // Emit Fix-Its.
    for fixIt in diagnostic.fixIts {
      diagnoseSingle(
        message: fixIt.message.message,
        severity: .note,
        node: diagnostic.node,
        position: diagnostic.position,
        fixItChanges: fixIt.changes
      )
    }

    // Emit any notes as follow-ons.
    for note in diagnostic.notes {
      diagnoseSingle(
        message: note.message,
        severity: .note,
        node: note.node,
        position: note.position
      )
    }
  }
}
