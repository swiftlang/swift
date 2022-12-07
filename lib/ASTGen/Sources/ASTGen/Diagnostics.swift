import CASTBridging
import SwiftDiagnostics
import SwiftSyntax

fileprivate func emitDiagnosticParts(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  sourceFileBuffer: UnsafeMutableBufferPointer<UInt8>,
  nodeStartOffset: Int?,
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
    at origPosition: AbsolutePosition
  ) -> UnsafeMutablePointer<UInt8>? {
    // FIXME: Our tree is very confused about absolute offsets. Work around
    // the issue in a very hacky way.
    let position: AbsolutePosition
    if let nodeStartOffset = nodeStartOffset,
        origPosition.utf8Offset < nodeStartOffset {
      position = origPosition + SourceLength(utf8Length: nodeStartOffset)
    } else {
      position = origPosition
    }

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
  nodeStartOffset: Int? = nil,
  diagnostic: Diagnostic
) {
  // Collect all of the Fix-It changes based on their Fix-It ID.
  var fixItChangesByID: [MessageID : [FixIt.Change]] = [:]
  for fixIt in diagnostic.fixIts {
    fixItChangesByID[fixIt.message.fixItID, default: []]
      .append(contentsOf: fixIt.changes.changes)
  }

  // Emit the main diagnostic
  emitDiagnosticParts(
    diagEnginePtr: diagEnginePtr,
    sourceFileBuffer: sourceFileBuffer,
    nodeStartOffset: nodeStartOffset,
    message: diagnostic.diagMessage.message,
    severity: diagnostic.diagMessage.severity,
    position: diagnostic.position,
    highlights: diagnostic.highlights,
    fixItChanges: fixItChangesByID[diagnostic.diagnosticID] ?? []
  )

  // Emit any notes as follow-ons.
  for note in diagnostic.notes {
    emitDiagnosticParts(
      diagEnginePtr: diagEnginePtr,
      sourceFileBuffer: sourceFileBuffer,
      nodeStartOffset: nodeStartOffset,
      message: note.message,
      severity: .note, position: note.position,
      fixItChanges: fixItChangesByID[note.noteMessage.fixItID] ?? []
    )
  }
}
