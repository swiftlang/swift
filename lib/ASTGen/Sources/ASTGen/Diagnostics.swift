import CASTBridging
import SwiftDiagnostics
import SwiftSyntax

fileprivate func emitDiagnosticParts(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  sourceFileBuffer: UnsafeMutableBufferPointer<UInt8>,
  message: String,
  severity: DiagnosticSeverity,
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

  // Form a source location for the given absolute position
  func sourceLoc(
    at position: AbsolutePosition
  ) -> UnsafeMutablePointer<UInt8>? {
    if let sourceFileBase = sourceFileBuffer.baseAddress,
      position.utf8Offset >= 0 &&
        position.utf8Offset < sourceFileBuffer.count {
      return sourceFileBase + position.utf8Offset
    }

    return nil
  }

  // Emit the diagnostic
  var mutableMessage = message
  let diag = mutableMessage.withUTF8 { messageBuffer in
    SwiftDiagnostic_create(
      diagEnginePtr, bridgedSeverity, sourceLoc(at: position),
      messageBuffer.baseAddress, messageBuffer.count
    )
  }

  // Emit highlights
  for highlight in highlights {
    SwiftDiagnostic_highlight(
      diag, sourceLoc(at: highlight.position),
      sourceLoc(at: highlight.endPosition)
    )
  }

  // Emit changes for a Fix-It.
  for change in fixItChanges {
    let replaceStartLoc: UnsafeMutablePointer<UInt8>?
    let replaceEndLoc: UnsafeMutablePointer<UInt8>?
    var newText: String

    switch change {
    case .replace(let oldNode, let newNode):
      replaceStartLoc = sourceLoc(at: oldNode.position)
      replaceEndLoc = sourceLoc(at: oldNode.endPosition)
      newText = newNode.description

    case .replaceLeadingTrivia(let oldToken, let newTrivia):
      replaceStartLoc = sourceLoc(at: oldToken.position)
      replaceEndLoc = sourceLoc(
        at: oldToken.positionAfterSkippingLeadingTrivia)
      newText = newTrivia.description

    case .replaceTrailingTrivia(let oldToken, let newTrivia):
      replaceStartLoc = sourceLoc(at: oldToken.endPositionBeforeTrailingTrivia)
      replaceEndLoc = sourceLoc(at: oldToken.endPosition)
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

/// Emit the given diagnostic via the diagnostic engine.
func emitDiagnostic(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  sourceFileBuffer: UnsafeMutableBufferPointer<UInt8>,
  diagnostic: Diagnostic,
  messageSuffix: String? = nil
) {
  // Emit the main diagnostic
  emitDiagnosticParts(
    diagEnginePtr: diagEnginePtr,
    sourceFileBuffer: sourceFileBuffer,
    message: diagnostic.diagMessage.message + (messageSuffix ?? ""),
    severity: diagnostic.diagMessage.severity,
    position: diagnostic.position,
    highlights: diagnostic.highlights
  )

  // Emit Fix-Its.
  for fixIt in diagnostic.fixIts {
    emitDiagnosticParts(
        diagEnginePtr: diagEnginePtr,
        sourceFileBuffer: sourceFileBuffer,
        message: fixIt.message.message,
        severity: .note, position: diagnostic.position,
        fixItChanges: fixIt.changes.changes
    )
  }

  // Emit any notes as follow-ons.
  for note in diagnostic.notes {
    emitDiagnosticParts(
      diagEnginePtr: diagEnginePtr,
      sourceFileBuffer: sourceFileBuffer,
      message: note.message,
      severity: .note, position: note.position
    )
  }
}
