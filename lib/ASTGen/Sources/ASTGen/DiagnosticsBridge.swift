//===--- DiagnosticsBridge.swift ------------------------------------------===//
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

import ASTBridging
import BasicBridging
import SwiftDiagnostics
import SwiftSyntax

fileprivate func emitDiagnosticParts(
  diagnosticEngine: BridgedDiagnosticEngine,
  sourceFileBuffer: UnsafeBufferPointer<UInt8>,
  message: String,
  severity: DiagnosticSeverity,
  position: AbsolutePosition,
  offset: Int,
  highlights: [Syntax] = [],
  fixItChanges: [FixIt.Change] = []
) {
  // Map severity
  let bridgedSeverity = severity.bridged

  func bridgedSourceLoc(at position: AbsolutePosition) -> BridgedSourceLoc {
    return BridgedSourceLoc(at: position.advanced(by: offset), in: sourceFileBuffer)
  }

  // Emit the diagnostic
  var mutableMessage = message
  let diag = mutableMessage.withBridgedString { bridgedMessage in
    BridgedDiagnostic(
      at: bridgedSourceLoc(at: position),
      message: bridgedMessage,
      severity: bridgedSeverity,
      engine: diagnosticEngine
    )
  }

  // Emit highlights
  for highlight in highlights {
    diag.highlight(
      start: bridgedSourceLoc(at: highlight.positionAfterSkippingLeadingTrivia),
      end: bridgedSourceLoc(at: highlight.endPositionBeforeTrailingTrivia)
    )
  }

  // Emit changes for a Fix-It.
  for change in fixItChanges {
    let replaceStartLoc: BridgedSourceLoc
    let replaceEndLoc: BridgedSourceLoc
    var newText: String

    switch change {
    case .replace(let oldNode, let newNode):
      replaceStartLoc = bridgedSourceLoc(at: oldNode.position)
      replaceEndLoc = bridgedSourceLoc(at: oldNode.endPosition)
      newText = newNode.description

    case .replaceLeadingTrivia(let oldToken, let newTrivia):
      replaceStartLoc = bridgedSourceLoc(at: oldToken.position)
      replaceEndLoc = bridgedSourceLoc(
        at: oldToken.positionAfterSkippingLeadingTrivia
      )
      newText = newTrivia.description

    case .replaceTrailingTrivia(let oldToken, let newTrivia):
      replaceStartLoc = bridgedSourceLoc(at: oldToken.endPositionBeforeTrailingTrivia)
      replaceEndLoc = bridgedSourceLoc(at: oldToken.endPosition)
      newText = newTrivia.description

#if RESILIENT_SWIFT_SYNTAX
    @unknown default:
      fatalError()
#endif
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

/// Emit the given diagnostic via the diagnostic engine.
func emitDiagnostic(
  diagnosticEngine: BridgedDiagnosticEngine,
  sourceFileBuffer: UnsafeBufferPointer<UInt8>,
  sourceFileBufferOffset: Int = 0,
  diagnostic: Diagnostic,
  diagnosticSeverity: DiagnosticSeverity,
  messageSuffix: String? = nil
) {
  // Emit the main diagnostic
  emitDiagnosticParts(
    diagnosticEngine: diagnosticEngine,
    sourceFileBuffer: sourceFileBuffer,
    message: diagnostic.diagMessage.message + (messageSuffix ?? ""),
    severity: diagnosticSeverity,
    position: diagnostic.position,
    offset: sourceFileBufferOffset,
    highlights: diagnostic.highlights
  )

  // Emit Fix-Its.
  for fixIt in diagnostic.fixIts {
    emitDiagnosticParts(
      diagnosticEngine: diagnosticEngine,
      sourceFileBuffer: sourceFileBuffer,
      message: fixIt.message.message,
      severity: .note,
      position: diagnostic.position,
      offset: sourceFileBufferOffset,
      fixItChanges: fixIt.changes
    )
  }

  // Emit any notes as follow-ons.
  for note in diagnostic.notes {
    emitDiagnosticParts(
      diagnosticEngine: diagnosticEngine,
      sourceFileBuffer: sourceFileBuffer,
      message: note.message,
      severity: .note,
      position: note.position,
      offset: sourceFileBufferOffset
    )
  }
}

extension DiagnosticSeverity {
  var bridged: BridgedDiagnosticSeverity {
    switch self {
    case .error: return .error
    case .note: return .note
    case .warning: return .warning
    case .remark: return .remark
#if RESILIENT_SWIFT_SYNTAX
    @unknown default: return .error
#endif
    }
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
    let bridgedSeverity = severity.bridged

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
      let replaceStartLoc: BridgedSourceLoc
      let replaceEndLoc: BridgedSourceLoc
      var newText: String

      switch change {
      case .replace(let oldNode, let newNode):
        replaceStartLoc = bridgedSourceLoc(
          for: oldNode,
          at: oldNode.positionAfterSkippingLeadingTrivia
        )
        replaceEndLoc = bridgedSourceLoc(
          for: oldNode,
          at: oldNode.endPositionBeforeTrailingTrivia
        )
        newText = newNode.description

      case .replaceLeadingTrivia(let oldToken, let newTrivia):
        replaceStartLoc = bridgedSourceLoc(for: oldToken)
        replaceEndLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.positionAfterSkippingLeadingTrivia
        )
        newText = newTrivia.description

      case .replaceTrailingTrivia(let oldToken, let newTrivia):
        replaceStartLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.endPositionBeforeTrailingTrivia
        )
        replaceEndLoc = bridgedSourceLoc(
          for: oldToken,
          at: oldToken.endPosition
        )
        newText = newTrivia.description

#if RESILIENT_SWIFT_SYNTAX
      @unknown default:
        fatalError()
#endif
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

struct QueuedDiagnostics {
  var grouped: GroupedDiagnostics = GroupedDiagnostics()

  /// The source file IDs we allocated, mapped from the buffer IDs used
  /// by the C++ source manager.
  var sourceFileIDs: [Int: UnsafeMutablePointer<GroupedDiagnostics.SourceFileID>] = [:]

  /// The known source files
  var sourceFiles: [ExportedSourceFile] = []
}

/// Create a grouped diagnostics structure in which we can add osou
@_cdecl("swift_ASTGen_createQueuedDiagnostics")
public func createQueuedDiagnostics() -> UnsafeRawPointer {
  let ptr = UnsafeMutablePointer<QueuedDiagnostics>.allocate(capacity: 1)
  ptr.initialize(to: .init())
  return UnsafeRawPointer(ptr)
}

/// Destroy the queued diagnostics.
@_cdecl("swift_ASTGen_destroyQueuedDiagnostics")
public func destroyQueuedDiagnostics(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.assumingMemoryBound(to: QueuedDiagnostics.self)
  for (_, sourceFileID) in queuedDiagnostics.pointee.sourceFileIDs {
    sourceFileID.deinitialize(count: 1)
    sourceFileID.deallocate()
  }

  queuedDiagnostics.deinitialize(count: 1)
  queuedDiagnostics.deallocate()
}

/// Diagnostic message used for thrown errors.
fileprivate struct SimpleDiagnostic: DiagnosticMessage {
  let message: String

  let severity: DiagnosticSeverity

  var diagnosticID: MessageID {
    .init(domain: "SwiftCompiler", id: "SimpleDiagnostic")
  }
}

extension BridgedDiagnosticSeverity {
  var asSeverity: DiagnosticSeverity {
    switch self {
    case .fatalError: return .error
    case .error: return .error
    case .warning: return .warning
    case .remark: return .remark
    case .note: return .note
    @unknown default: return .error
    }
  }
}

/// Register a source file wih the queued diagnostics.
@_cdecl("swift_ASTGen_addQueuedSourceFile")
public func addQueuedSourceFile(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer,
  bufferID: Int,
  sourceFilePtr: UnsafeRawPointer,
  displayNamePtr: UnsafePointer<UInt8>,
  displayNameLength: Int,
  parentID: Int,
  positionInParent: Int
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.assumingMemoryBound(to: QueuedDiagnostics.self)
  // Determine the parent link, for a child buffer.
  let parent: (GroupedDiagnostics.SourceFileID, AbsolutePosition)?
  if parentID >= 0,
    let parentSourceFileID = queuedDiagnostics.pointee.sourceFileIDs[parentID]
  {
    parent = (parentSourceFileID.pointee, AbsolutePosition(utf8Offset: positionInParent))
  } else {
    parent = nil
  }

  let displayName = String(
    decoding: UnsafeBufferPointer(
      start: displayNamePtr,
      count: displayNameLength
    ),
    as: UTF8.self
  )

  // Add the source file.
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let sourceFileID = queuedDiagnostics.pointee.grouped.addSourceFile(
    tree: sourceFile.pointee.syntax,
    sourceLocationConverter: sourceFile.pointee.sourceLocationConverter,
    displayName: displayName,
    parent: parent
  )
  queuedDiagnostics.pointee.sourceFiles.append(sourceFile.pointee)

  // Record the buffer ID.
  let allocatedSourceFileID = UnsafeMutablePointer<GroupedDiagnostics.SourceFileID>.allocate(capacity: 1)
  allocatedSourceFileID.initialize(to: sourceFileID)
  queuedDiagnostics.pointee.sourceFileIDs[bufferID] = allocatedSourceFileID
}

/// Add a new diagnostic to the queue.
@_cdecl("swift_ASTGen_addQueuedDiagnostic")
public func addQueuedDiagnostic(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer,
  text: UnsafePointer<UInt8>,
  textLength: Int,
  severity: BridgedDiagnosticSeverity,
  position: BridgedSourceLoc,
  highlightRangesPtr: UnsafePointer<BridgedSourceLoc>?,
  numHighlightRanges: Int
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.assumingMemoryBound(
    to: QueuedDiagnostics.self
  )

  guard let rawPosition = position.getOpaquePointerValue() else {
    return
  }

  // Find the source file that contains this location.
  let sourceFile = queuedDiagnostics.pointee.sourceFiles.first { sf in
    guard let baseAddress = sf.buffer.baseAddress else {
      return false
    }

    return rawPosition >= baseAddress && rawPosition < baseAddress + sf.buffer.count
  }
  guard let sourceFile = sourceFile else {
    // FIXME: Hard to report an error here...
    return
  }

  // Find the token at that offset.
  let sourceFileBaseAddress = UnsafeRawPointer(sourceFile.buffer.baseAddress!)
  let sourceFileEndAddress = sourceFileBaseAddress + sourceFile.buffer.count
  let offset = rawPosition - sourceFileBaseAddress
  guard let token = sourceFile.syntax.token(at: AbsolutePosition(utf8Offset: offset)) else {
    return
  }

  // Map the highlights.
  var highlights: [Syntax] = []
  let highlightRanges = UnsafeBufferPointer<BridgedSourceLoc>(
    start: highlightRangesPtr,
    count: numHighlightRanges * 2
  )
  for index in 0..<numHighlightRanges {
    // Make sure both the start and the end land within this source file.
    guard let start = highlightRanges[index * 2].getOpaquePointerValue(),
      let end = highlightRanges[index * 2 + 1].getOpaquePointerValue()
    else {
      continue
    }

    guard start >= sourceFileBaseAddress && start < sourceFileEndAddress,
      end >= sourceFileBaseAddress && end <= sourceFileEndAddress
    else {
      continue
    }

    // Find start tokens in the source file.
    let startPos = AbsolutePosition(utf8Offset: start - sourceFileBaseAddress)
    guard let startToken = sourceFile.syntax.token(at: startPos) else {
      continue
    }

    // Walk up from the start token until we find a syntax node that matches
    // the highlight range.
    let endPos = AbsolutePosition(utf8Offset: end - sourceFileBaseAddress)
    var highlightSyntax = Syntax(startToken)
    while true {
      // If this syntax matches our starting/ending positions, add the
      // highlight and we're done.
      if highlightSyntax.positionAfterSkippingLeadingTrivia == startPos
        && highlightSyntax.endPositionBeforeTrailingTrivia == endPos
      {
        highlights.append(highlightSyntax)
        break
      }

      // Go up to the parent.
      guard let parent = highlightSyntax.parent else {
        break
      }

      highlightSyntax = parent
    }
  }

  let textBuffer = UnsafeBufferPointer(start: text, count: textLength)
  let diagnostic = Diagnostic(
    node: Syntax(token),
    message: SimpleDiagnostic(
      message: String(decoding: textBuffer, as: UTF8.self),
      severity: severity.asSeverity
    ),
    highlights: highlights
  )

  queuedDiagnostics.pointee.grouped.addDiagnostic(diagnostic)
}

/// Render the queued diagnostics into a UTF-8 string.
@_cdecl("swift_ASTGen_renderQueuedDiagnostics")
public func renderQueuedDiagnostics(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer,
  contextSize: Int,
  colorize: Int,
  renderedStringOutPtr: UnsafeMutablePointer<BridgedStringRef>
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.assumingMemoryBound(to: QueuedDiagnostics.self)
  let formatter = DiagnosticsFormatter(contextSize: contextSize, colorize: colorize != 0)
  let renderedStr = formatter.annotateSources(in: queuedDiagnostics.pointee.grouped)

  renderedStringOutPtr.pointee = allocateBridgedString(renderedStr)
}
