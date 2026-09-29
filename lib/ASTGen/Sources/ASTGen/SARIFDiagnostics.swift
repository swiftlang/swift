//===--- SARIFDiagnostics.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Serialization of queued compiler diagnostics to the Static Analysis Results
// Interchange Format (SARIF) v2.1.0.
//
//===----------------------------------------------------------------------===//

import BasicBridging
import Foundation
import SARIF
@_spi(Compiler) import SwiftDiagnostics
import SwiftSyntax

extension DiagnosticSeverity {
  /// The SARIF result kind and level for this severity.
  ///
  /// SARIF requires 'level' to be "none" whenever 'kind' is not "fail", so a
  /// remark maps to an informational result rather than carrying a level.
  fileprivate var sarifKind: Result.Kind {
    switch self {
    case .error: return .fail(level: .error)
    case .warning: return .fail(level: .warning)
    case .remark: return .informational
    case .note: return .fail(level: .note)
    }
  }
}

extension ExportedSourceFile {
  /// The 1-based UTF-16 code unit column of \c position within its line.
  ///
  /// 'SourceLocationConverter' reports a column as a UTF-8 byte offset, while
  /// SARIF measures it in the unit named by the run's 'columnKind'. The two
  /// agree only for lines that are entirely ASCII, so the bytes preceding the
  /// position on its line have to be re-counted.
  fileprivate func utf16Column(
    of position: AbsolutePosition, utf8Column: Int
  ) -> Int {
    // 'utf8Column' is 1-based, so the line begins that many bytes back.
    let lineStart = position.utf8Offset - (utf8Column - 1)
    guard lineStart >= 0, position.utf8Offset <= buffer.count else {
      return utf8Column
    }

    let linePrefix = buffer[lineStart..<position.utf8Offset]
    return String(decoding: linePrefix, as: UTF8.self).utf16.count + 1
  }
}

/// Builds a SARIF log from a set of queued diagnostics.
private final class SARIFLogBuilder {
  let log: SARIFLog
  let driver: ToolComponent
  let run: Run

  /// Artifacts created so far, keyed by index into
  /// 'QueuedDiagnostics.sourceFiles'.
  private var artifacts: [Int: Artifact] = [:]

  /// Rules created so far, keyed by diagnostic identifier. Rules must be
  /// registered with the tool component rather than constructed directly, so
  /// that the run's 'rules' array and each result's rule index stay consistent.
  private var rules: [String: Rule] = [:]

  /// Location converters, keyed as 'artifacts' is. 'ExportedSourceFile' builds
  /// one lazily, so it is cached here to build each file's line table once
  /// rather than once per diagnostic.
  private var converters: [Int: SourceLocationConverter] = [:]

  init(compilerVersion: String) {
    let driver = ToolComponent(named: "Swift Compiler")
    driver.version = compilerVersion
    driver.informationUri = URL(string: "https://swift.org")
    driver.organization = "Swift Project"

    let log = SARIFLog(version: .v2_1_0)
    let run = log.addRun(tool: Tool(driver: driver))

    // SARIF measures columns in the unit named here. Consumers disagree on what
    // the default is when it is absent, so state it rather than imply it.
    run.columnKind = .utf16CodeUnits

    self.driver = driver
    self.log = log
    self.run = run
  }

  /// The artifact for a source file, creating it on first use so that files
  /// without diagnostics do not appear in the log.
  private func artifact(
    forSourceFileAt index: Int, in sourceFiles: [ExportedSourceFile]
  ) -> Artifact {
    if let existing = artifacts[index] {
      return existing
    }

    let artifact = run.addArtifact()
    artifact.location = ArtifactLocation(
      uri: URL(fileURLWithPath: sourceFiles[index].fileName), uriBaseId: nil)
    artifact.sourceLanguage = "swift"
    artifacts[index] = artifact

    return artifact
  }

  private func rule(id: String) -> Rule {
    if let existing = rules[id] {
      return existing
    }

    let rule = driver.addRule(id: id)
    rules[id] = rule

    return rule
  }

  /// The location converter for a source file, built on first use.
  private func converter(
    forSourceFileAt index: Int, in sourceFiles: [ExportedSourceFile]
  ) -> SourceLocationConverter {
    if let existing = converters[index] {
      return existing
    }

    // 'sourceLocationConverter' is lazy, so it needs a mutable value.
    var sourceFile = sourceFiles[index]
    let converter = sourceFile.sourceLocationConverter
    converters[index] = converter

    return converter
  }

  /// The SARIF location for a diagnostic at \c position in the source file at
  /// \c sourceFileIndex.
  func location(
    at position: AbsolutePosition, inSourceFileAt sourceFileIndex: Int,
    in sourceFiles: [ExportedSourceFile], message: Message? = nil
  ) -> Location {
    let sourceFile = sourceFiles[sourceFileIndex]
    let converted = converter(
      forSourceFileAt: sourceFileIndex, in: sourceFiles).location(for: position)
    let column = sourceFile.utf16Column(
      of: position, utf8Column: converted.column)

    let artifact = self.artifact(
      forSourceFileAt: sourceFileIndex, in: sourceFiles)

    let region = Region(
      text: TextRegion(line: Int32(converted.line), column: Int32(column)))

    return Location(
      at: PhysicalLocation(
        artifactLocation: ArtifactLocationReference(to: artifact),
        region: region),
      message: message)
  }

  /// Add a result for a diagnostic.
  ///
  /// The caller attaches the locations, since a diagnostic raised without a
  /// source location has none.
  @discardableResult
  func addResult(for message: any DiagnosticMessage) -> Result {
    let result = run.addResult(
      rule: rule(id: message.diagnosticID.id), messageText: message.message)
    result.kind = message.severity.sarifKind

    return result
  }
}

/// Render the queued diagnostics as a SARIF log.
///
/// Returns true on success, writing the log into 'renderedString'. On failure,
/// writes a message into 'errorMessageOut' and returns false.
@_cdecl("swift_ASTGen_renderQueuedDiagnosticsAsSARIF")
public func renderQueuedDiagnosticsAsSARIF(
  queuedDiagnosticsPtr: UnsafeMutableRawPointer?,
  compilerVersion: BridgedStringRef,
  renderedString: UnsafeMutablePointer<BridgedStringRef>,
  errorMessageOut: UnsafeMutablePointer<BridgedStringRef>
) -> Bool {
  let queuedDiagnostics = queuedDiagnosticsPtr?.assumingMemoryBound(
    to: QueuedDiagnostics.self
  )

  let builder = SARIFLogBuilder(
    compilerVersion: String(bridged: compilerVersion))

  if let queued = queuedDiagnostics?.pointee {
    // 'sourceFileIDs' is in the order the source files were added, and
    // 'addQueuedSourceFile' appends to 'sourceFiles' in that same order.
    // Check if this invariant is ever violated.
    precondition(
      queued.grouped.sourceFileIDs.count == queued.sourceFiles.count,
      "source files registered with 'grouped' and with 'sourceFiles' disagree")

    for (sourceFileIndex, sourceFileID) in queued.grouped.sourceFileIDs.enumerated() {
      for diagnostic in queued.grouped.diagnostics(in: sourceFileID) {
        builder.addResult(for: diagnostic.diagMessage).locations = [
          builder.location(
            at: diagnostic.position, inSourceFileAt: sourceFileIndex,
            in: queued.sourceFiles)
        ]
      }
    }

    for message in queued.unlocated {
      builder.addResult(for: message)
    }
  }

  do {
    let json = try builder.log.toJSONString(formatting: .pretty) + "\n"
    renderedString.pointee = allocateBridgedString(json)
  } catch {
    errorMessageOut.pointee = allocateBridgedString(
      "could not serialize SARIF diagnostics: \(error)")
    return false
  }

  return true
}
