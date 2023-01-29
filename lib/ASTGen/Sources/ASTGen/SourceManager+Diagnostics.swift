import CASTBridging
import SwiftDiagnostics
import SwiftSyntax

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
    let bridgedSeverity: BridgedDiagnosticSeverity
    switch severity {
      case .error: bridgedSeverity = .error
      case .note: bridgedSeverity = .note
      case .warning: bridgedSeverity = .warning
    }

    // Emit the diagnostic
    var mutableMessage = message
    let diag = mutableMessage.withUTF8 { messageBuffer in
      SwiftDiagnostic_create(
        cxxDiagnosticEngine, bridgedSeverity,
        cxxSourceLocation(for: node, at: position),
        messageBuffer.baseAddress, messageBuffer.count
      )
    }

    // Emit highlights
    for highlight in highlights {
      SwiftDiagnostic_highlight(
        diag,
        cxxSourceLocation(for: highlight),
        cxxSourceLocation(for: highlight, at: highlight.endPosition)
      )
    }

    // Emit changes for a Fix-It.
    for change in fixItChanges {
      let replaceStartLoc: CxxSourceLoc?
      let replaceEndLoc: CxxSourceLoc?
      var newText: String

      switch change {
      case .replace(let oldNode, let newNode):
        replaceStartLoc = cxxSourceLocation(for: oldNode)
        replaceEndLoc = cxxSourceLocation(
          for: oldNode,
          at: oldNode.endPosition
        )
        newText = newNode.description

      case .replaceLeadingTrivia(let oldToken, let newTrivia):
        replaceStartLoc = cxxSourceLocation(for: oldToken)
        replaceEndLoc = cxxSourceLocation(
          for: oldToken,
          at: oldToken.positionAfterSkippingLeadingTrivia
        )
        newText = newTrivia.description

      case .replaceTrailingTrivia(let oldToken, let newTrivia):
        replaceStartLoc = cxxSourceLocation(
          for: oldToken,
          at: oldToken.endPositionBeforeTrailingTrivia)
        replaceEndLoc = cxxSourceLocation(
          for: oldToken,
          at: oldToken.endPosition
        )
        newText = newTrivia.description
      }

      newText.withUTF8 { textBuffer in
        SwiftDiagnostic_fixItReplace(
          diag, replaceStartLoc, replaceEndLoc,
          textBuffer.baseAddress, textBuffer.count
        )
      }
    }

    SwiftDiagnostic_finish(diag);
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
          fixItChanges: fixIt.changes.changes
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
