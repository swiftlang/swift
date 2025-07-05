//===--- DiagnosticsBridge.swift ------------------------------------------===//
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
import SwiftDiagnostics
import SwiftSyntax

fileprivate struct PerFrontendDiagnosticState {
  /// The set of categories that were referenced by a diagnostic.
  var referencedCategories: Set<DiagnosticCategory> = []
}

fileprivate func emitDiagnosticParts(
  diagnosticEngine: BridgedDiagnosticEngine,
  sourceFileBuffer: UnsafeBufferPointer<UInt8>,
  message: String,
  severity: DiagnosticSeverity,
  position: AbsolutePosition,
  offset: Int,
  highlights: [Syntax] = [],
  edits: [SourceEdit] = []
) {
  // Map severity
  let bridgedSeverity = severity.bridged

  func sourceLoc(at position: AbsolutePosition) -> SourceLoc {
    return SourceLoc(at: position.advanced(by: offset), in: sourceFileBuffer)
  }

  // Emit the diagnostic
  var mutableMessage = message
  let diag = mutableMessage.withBridgedString { bridgedMessage in
    BridgedDiagnostic(
      at: sourceLoc(at: position),
      message: bridgedMessage,
      severity: bridgedSeverity,
      engine: diagnosticEngine
    )
  }

  // Emit highlights
  for highlight in highlights {
    diag.highlight(
      start: sourceLoc(at: highlight.positionAfterSkippingLeadingTrivia),
      end: sourceLoc(at: highlight.endPositionBeforeTrailingTrivia)
    )
  }

  // Emit changes for a Fix-It.
  for edit in edits {
    var newText: String = edit.replacement
    newText.withBridgedString { bridgedMessage in
      diag.fixItReplace(
        start: sourceLoc(at: edit.range.lowerBound),
        end: sourceLoc(at: edit.range.upperBound),
        replacement: bridgedMessage
      )
    }
  }

  diag.finish();
}

/// Emit the given diagnostic via the diagnostic engine.
public func emitDiagnostic(
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
  // FIXME: Ths assumes the fixIt is on the same tree/buffer, which is not guaranteed.
  for fixIt in diagnostic.fixIts {
    emitDiagnosticParts(
      diagnosticEngine: diagnosticEngine,
      sourceFileBuffer: sourceFileBuffer,
      message: fixIt.message.message,
      severity: .note,
      position: diagnostic.position,
      offset: sourceFileBufferOffset,
      edits: fixIt.edits
    )
  }

  // Emit any notes as follow-ons.
  // FIXME: Ths assumes the node is on the same tree/buffer, which is not guaranteed.
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
  public var bridged: swift.DiagnosticKind {
    switch self {
    case .error: return .error
    case .note: return .note
    case .warning: return .warning
    case .remark: return .remark
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

  let category: DiagnosticCategory?

  var diagnosticID: MessageID {
    .init(domain: "SwiftCompiler", id: "SimpleDiagnostic")
  }
}

extension swift.DiagnosticKind {
  var asSeverity: DiagnosticSeverity {
    switch self {
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

private struct BridgedFixItMessage: FixItMessage {
  var message: String { "" }

  var fixItID: MessageID {
    .init(domain: "SwiftCompiler", id: "BridgedFixIt")
  }
}

/// Add a new diagnostic to the queue.
@_cdecl("swift_ASTGen_addQueuedDiagnostic")
public func addQueuedDiagnostic(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer,
  perFrontendDiagnosticStatePtr: UnsafeMutableRawPointer,
  text: BridgedStringRef,
  severity: swift.DiagnosticKind,
  loc: SourceLoc,
  categoryName: BridgedStringRef,
  documentationPath: BridgedStringRef,
  highlightRangesPtr: UnsafePointer<CharSourceRange>?,
  numHighlightRanges: Int,
  fixItsUntyped: BridgedArrayRef
) {
  let queuedDiagnostics = queuedDiagnosticsPtr.assumingMemoryBound(
    to: QueuedDiagnostics.self
  )

  let diagnosticState = perFrontendDiagnosticStatePtr.assumingMemoryBound(
    to: PerFrontendDiagnosticState.self
  )

  guard let rawPosition = loc.raw else {
    return
  }

  // Find the source file that contains this location.
  let sourceFile = queuedDiagnostics.pointee.sourceFiles.first { sf in
    guard let baseAddress = sf.buffer.baseAddress else {
      return false
    }

    return rawPosition >= baseAddress && rawPosition <= baseAddress + sf.buffer.count
  }
  guard let sourceFile = sourceFile else {
    // FIXME: Hard to report an error here...
    return
  }

  let sourceFileBaseAddress = UnsafeRawPointer(sourceFile.buffer.baseAddress!)
  let sourceFileEndAddress = sourceFileBaseAddress + sourceFile.buffer.count
  let offset = rawPosition - sourceFileBaseAddress
  let position = AbsolutePosition(utf8Offset: offset)

  // Find the token at that offset.
  let node: Syntax
  if let token = sourceFile.syntax.token(at: position) {
    node = Syntax(token)
  } else if position == sourceFile.syntax.endPosition {
    // FIXME: EOF token is not included in '.token(at: position)'
    // We might want to include it, but want to avoid special handling.
    // Also 'sourceFile.syntax' is not guaranteed to be 'SourceFileSyntax'.
    if let token = sourceFile.syntax.lastToken(viewMode: .all) {
      node = Syntax(token)
    } else {
      node = sourceFile.syntax
    }
  } else {
    // position out of range.
    return
  }

  // Map the highlights.
  var highlights: [Syntax] = []
  let highlightRanges = UnsafeBufferPointer<CharSourceRange>(
    start: highlightRangesPtr,
    count: numHighlightRanges
  )
  for index in 0..<numHighlightRanges {
    let range = highlightRanges[index]

    // Make sure both the start and the end land within this source file.
    guard let start = range.start.raw, let end = range.end.raw else {
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

  let documentationPath = String(bridged: documentationPath)
  let documentationURL: String? = if !documentationPath.isEmpty {
      // If this looks doesn't look like a URL, prepend file://.
      documentationPath.looksLikeURL ? documentationPath : "file://\(documentationPath)"
    } else {
      nil
    }

  let categoryName = String(bridged: categoryName)
  // If the data comes from serialized diagnostics, it's possible that
  // the category name is empty because StringRef() is serialized into
  // an empty string.
  let category: DiagnosticCategory? = if !categoryName.isEmpty {
      DiagnosticCategory(
        name: categoryName,
        documentationURL: documentationURL
      )
    } else {
      nil
    }

  // Note that we referenced this category.
  if let category {
    diagnosticState.pointee.referencedCategories.insert(category)
  }

  // Map the Fix-Its
  let fixItChanges: [FixIt.Change] = fixItsUntyped.withElements(ofType: BridgedFixIt.self) { fixIts in
    fixIts.compactMap { fixIt in
      guard let startPos = sourceFile.position(of: fixIt.replacementRange.start),
            let endPos = sourceFile.position(of: fixIt.replacementRange.end) else {
        return nil
      }

      return FixIt.Change.replaceText(
        range: startPos..<endPos,
        with: String(bridged: fixIt.replacementText),
        in: sourceFile.syntax
      )
    }
  }

  let fixIts: [FixIt] = fixItChanges.isEmpty
      ? []
      : [
          FixIt(
            message: BridgedFixItMessage(),
            changes: fixItChanges
          )
        ]

  let diagnostic = Diagnostic(
    node: node,
    position: position,
    message: SimpleDiagnostic(
      message: String(bridged: text),
      severity: severity.asSeverity,
      category: category
    ),
    highlights: highlights,
    fixIts: fixIts
  )

  queuedDiagnostics.pointee.grouped.addDiagnostic(diagnostic)
}

/// Render a single diagnostic that has no source location information.
@_cdecl("swift_ASTGen_renderSingleDiagnostic")
public func renderSingleDiagnostic(
  perFrontendDiagnosticStatePtr: UnsafeMutableRawPointer,
  text: BridgedStringRef,
  severity: swift.DiagnosticKind,
  categoryName: BridgedStringRef,
  documentationPath: BridgedStringRef,
  colorize: Int,
  renderedStringOutPtr: UnsafeMutablePointer<BridgedStringRef>
) {
  let diagnosticState = perFrontendDiagnosticStatePtr.assumingMemoryBound(
    to: PerFrontendDiagnosticState.self
  )

  let documentationPath = String(bridged: documentationPath)
  let documentationURL: String? = if !documentationPath.isEmpty {
      // If this looks doesn't look like a URL, prepend file://.
      documentationPath.looksLikeURL ? documentationPath : "file://\(documentationPath)"
    } else {
      nil
    }

  let categoryName = String(bridged: categoryName)
  // If the data comes from serialized diagnostics, it's possible that
  // the category name is empty because StringRef() is serialized into
  // an empty string.
  let category: DiagnosticCategory? = if !categoryName.isEmpty {
      DiagnosticCategory(
        name: categoryName,
        documentationURL: documentationURL
      )
    } else {
      nil
    }

  // Note that we referenced this category.
  if let category {
    diagnosticState.pointee.referencedCategories.insert(category)
  }

  let formatter = DiagnosticsFormatter(colorize: colorize != 0)

  let renderedStr = formatter.formattedMessage(
    SimpleDiagnostic(
      message: String(bridged: text),
      severity: severity.asSeverity,
      category: category
    )
  )

  renderedStringOutPtr.pointee = allocateBridgedString(renderedStr)
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

extension String {
  /// Simple check to determine whether the string looks like the start of a
  /// URL.
  fileprivate var looksLikeURL: Bool {
    var sawColon: Bool = false
    var forwardSlashes: Int = 0
    for c in self {
      if c == ":" {
        sawColon = true
        continue
      }

      if c == "/" && sawColon {
        forwardSlashes += 1
        if forwardSlashes >= 2 {
          return true
        }

        continue
      }

      if c.isLetter || c.isNumber {
        forwardSlashes = 0
        sawColon = false
        continue
      }

      return false
    }

    return false
  }
}

@_cdecl("swift_ASTGen_createPerFrontendDiagnosticState")
public func createPerFrontendDiagnosticState() -> UnsafeMutableRawPointer {
  let ptr = UnsafeMutablePointer<PerFrontendDiagnosticState>.allocate(capacity: 1)
  ptr.initialize(to: .init())
  return UnsafeMutableRawPointer(ptr)
}

@_cdecl("swift_ASTGen_destroyPerFrontendDiagnosticState")
public func destroyPerFrontendDiagnosticState(
  statePtr: UnsafeMutableRawPointer
) {
  let state = statePtr.assumingMemoryBound(to: PerFrontendDiagnosticState.self)
  state.deinitialize(count: 1)
  state.deallocate()
}

@_cdecl("swift_ASTGen_renderCategoryFootnotes")
public func renderCategoryFootnotes(
  statePtr: UnsafeMutableRawPointer,
  colorize: Int,
  renderedStringOutPtr: UnsafeMutablePointer<BridgedStringRef>
) {
  let state = statePtr.assumingMemoryBound(to: PerFrontendDiagnosticState.self)
  let formatter = DiagnosticsFormatter(contextSize: 0, colorize: colorize != 0)
  var renderedStr = formatter.categoryFootnotes(
    Array(state.pointee.referencedCategories),
    leadingText: "\n"
  )

  if !renderedStr.isEmpty {
    renderedStr += "\n"
  }

  renderedStringOutPtr.pointee = allocateBridgedString(renderedStr)

  // Clear out categories so we start fresh.
  state.pointee.referencedCategories = []
}
