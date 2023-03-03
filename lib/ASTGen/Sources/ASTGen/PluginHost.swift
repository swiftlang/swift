//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CASTBridging
import CBasicBridging
import SwiftSyntax

enum PluginError: Error {
  case failedToSendMessage
  case failedToReceiveMessage
  case invalidReponseKind
}

@_cdecl("swift_ASTGen_initializePlugin")
public func _initializePlugin(
  opaqueHandle: UnsafeMutableRawPointer
) {
  let plugin = CompilerPlugin(opaqueHandle: opaqueHandle)
  plugin.initialize()
}

@_cdecl("swift_ASTGen_deinitializePlugin")
public func _deinitializePlugin(
  opaqueHandle: UnsafeMutableRawPointer
) {
  let plugin =  CompilerPlugin(opaqueHandle: opaqueHandle)
  plugin.deinitialize()
}

struct CompilerPlugin {
  let opaqueHandle: UnsafeMutableRawPointer

  private func withLock<R>(_ body: () throws -> R) rethrows -> R {
    Plugin_lock(opaqueHandle)
    defer { Plugin_unlock(opaqueHandle) }
    return try body()
  }

  private func sendMessage(_ message: HostToPluginMessage) throws {
    let hadError = try LLVMJSON.encoding(message) { (data) -> Bool in
//      // FIXME: Add -dump-plugin-message option?
//      data.withMemoryRebound(to: UInt8.self) { buffer in
//        print(">> " + String(decoding: buffer, as: UTF8.self))
//      }
      return Plugin_sendMessage(opaqueHandle, BridgedData(baseAddress: data.baseAddress, size: data.count))
    }
    if hadError {
      throw PluginError.failedToSendMessage
    }
  }

  private func waitForNextMessage() throws -> PluginToHostMessage {
    var result: BridgedData = BridgedData()
    let hadError = Plugin_waitForNextMessage(opaqueHandle, &result)
    defer { BridgedData_free(result) }
    guard !hadError else {
      throw PluginError.failedToReceiveMessage
    }
    let data = UnsafeBufferPointer(start: result.baseAddress, count: result.size)
//    // FIXME: Add -dump-plugin-message option?
//    data.withMemoryRebound(to: UInt8.self) { buffer in
//      print("<< " + String(decoding: buffer, as: UTF8.self))
//    }
    return try LLVMJSON.decode(PluginToHostMessage.self, from: data)
  }

  func sendMessageAndWait(_ message: HostToPluginMessage) throws -> PluginToHostMessage {
    try self.withLock {
      try sendMessage(message)
      return try waitForNextMessage()
    }
  }

  func initialize() {
    self.withLock {
      // Get capability.
      let response: PluginToHostMessage
      do {
        try self.sendMessage(.getCapability)
        response = try self.waitForNextMessage()
      } catch {
        assertionFailure(String(describing: error))
        return
      }
      switch response {
      case .getCapabilityResult(capability: let capability):
        let ptr = UnsafeMutablePointer<PluginMessage.PluginCapability>.allocate(capacity: 1)
        ptr.initialize(to: capability)
        Plugin_setCapability(opaqueHandle, UnsafeRawPointer(ptr))
      default:
        assertionFailure("invalid response")
      }
    }
  }
  func deinitialize() {
    self.withLock {
      if let ptr = Plugin_getCapability(opaqueHandle) {
        let capabilityPtr = UnsafeMutableRawPointer(mutating: ptr)
          .assumingMemoryBound(to: PluginMessage.PluginCapability.self)
        capabilityPtr.deinitialize(count: 1)
        capabilityPtr.deallocate()
      }
    }
  }

  var capability: PluginMessage.PluginCapability {
    if let ptr = Plugin_getCapability(opaqueHandle) {
      return ptr.assumingMemoryBound(to: PluginMessage.PluginCapability.self).pointee
    }
    return PluginMessage.PluginCapability(protocolVersion: 0)
  }
}

class PluginDiagnosticsEngine {
  private let cxxDiagnosticEngine: UnsafeMutablePointer<UInt8>
  private var exportedSourceFileByName: [String: UnsafePointer<ExportedSourceFile>] = [:]

  init(cxxDiagnosticEngine: UnsafeMutablePointer<UInt8>) {
    self.cxxDiagnosticEngine = cxxDiagnosticEngine
  }

  /// Register an 'ExportedSourceFile' to the engine. So the engine can get
  /// C++ SourceLoc from a pair of filename and offset.
  func add(exportedSourceFile: UnsafePointer<ExportedSourceFile>) {
    exportedSourceFileByName[exportedSourceFile.pointee.fileName] = exportedSourceFile
  }

  /// Emit a diagnostic to C++ diagnostic engine. Note that one plugin
  /// diagnostic can emits multiple C++ diagnostics.
  func emit(
    _ diagnostic: PluginMessage.Diagnostic,
    messageSuffix: String? = nil
  ) {

    // Emit the main diagnostic.
    emitSingle(
      message: diagnostic.message + (messageSuffix ?? ""),
      severity: diagnostic.severity,
      position: diagnostic.position,
      highlights: diagnostic.highlights)

    // Emit Fix-Its.
    for fixIt in diagnostic.fixIts {
      emitSingle(
        message: fixIt.message,
        severity: .note,
        position: diagnostic.position,
        fixItChanges: fixIt.changes)
    }

    // Emit any notes as follow-ons.
    for note in diagnostic.notes {
      emitSingle(
        message: note.message,
        severity: .note,
        position: note.position)
    }
  }
  /// Emit single C++ diagnostic.
  private func emitSingle(
    message: String,
    severity: PluginMessage.Diagnostic.Severity,
    position: PluginMessage.Diagnostic.Position,
    highlights: [PluginMessage.Diagnostic.PositionRange] = [],
    fixItChanges: [PluginMessage.Diagnostic.FixIt.Change] = []
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
        cxxSourceLocation(at: position),
        messageBuffer.baseAddress, messageBuffer.count)
    }

    // Emit highlights
    for highlight in highlights {
      guard let (startLoc, endLoc) = cxxSourceRange(for: highlight) else {
        continue
      }
      SwiftDiagnostic_highlight(diag, startLoc, endLoc)
    }

    // Emit changes for a Fix-It.
    for change in fixItChanges {
      guard let (startLoc, endLoc) = cxxSourceRange(for: change.range) else {
        continue
      }
      var newText = change.newText
      newText.withUTF8 { textBuffer in
        SwiftDiagnostic_fixItReplace(
          diag, startLoc, endLoc, textBuffer.baseAddress, textBuffer.count)
      }
    }

    SwiftDiagnostic_finish(diag)
  }

  /// Emit diagnostics.
  func emit(
    _ diagnostics: [PluginMessage.Diagnostic],
    messageSuffix: String? = nil
  ) {
    for diagnostic in diagnostics {
      self.emit(diagnostic)
    }
  }

  /// Produce the C++ source location for a given position based on a
  /// syntax node.
  private func cxxSourceLocation(
    at offset: Int, in fileName: String
  ) -> CxxSourceLoc? {
    // Find the corresponding exported source file.
    guard
      let exportedSourceFile = exportedSourceFileByName[fileName]
    else {
      return nil
    }

    // Compute the resulting address.
    guard
      let bufferBaseAddress = exportedSourceFile.pointee.buffer.baseAddress
    else {
      return nil
    }
    return bufferBaseAddress.advanced(by: offset)
  }

  /// C++ source location from a position value from a plugin.
  private func cxxSourceLocation(
    at position: PluginMessage.Diagnostic.Position
  ) -> CxxSourceLoc? {
    cxxSourceLocation(at: position.offset, in: position.fileName)
  }

  /// C++ source range from a range value from a plugin.
  private func cxxSourceRange(
    for range: PluginMessage.Diagnostic.PositionRange
  ) -> (start: CxxSourceLoc, end: CxxSourceLoc)? {
    guard
      let start = cxxSourceLocation(at: range.startOffset, in: range.fileName),
      let end = cxxSourceLocation(at: range.endOffset, in: range.fileName)
    else {
      return nil
    }
    return (start: start, end: end )
  }
}

extension PluginMessage.Syntax {
  init?(syntax: Syntax, in sourceFilePtr: UnsafePointer<ExportedSourceFile>) {
    let kind: PluginMessage.Syntax.Kind
    switch true {
    case syntax.is(DeclSyntax.self): kind = .declaration
    case syntax.is(ExprSyntax.self): kind = .expression
    case syntax.is(StmtSyntax.self): kind = .statement
    case syntax.is(TypeSyntax.self): kind = .type
    case syntax.is(PatternSyntax.self): kind = .pattern
    case syntax.is(AttributeSyntax.self): kind = .attribute
    default: return nil
    }
    let source = syntax.description


    let sourceStr = String(decoding: sourceFilePtr.pointee.buffer, as: UTF8.self)
    let fileName = sourceFilePtr.pointee.fileName
    let fileID = "\(sourceFilePtr.pointee.moduleName)/\(sourceFilePtr.pointee.fileName.basename)"
    let converter = SourceLocationConverter(file: fileName, source: sourceStr)
    let loc = converter.location(for: syntax.position)

    self.init(
      kind: kind,
      source: source,
      location: .init(
        fileID: fileID,
        fileName: fileName,
        offset: loc.offset,
        line: loc.line!,
        column: loc.column!))
  }
}
