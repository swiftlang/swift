//===--- SourceFile.swift -------------------------------------------------===//
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
import SwiftDiagnostics
import SwiftIfConfig
@_spi(ExperimentalLanguageFeatures) import SwiftParser
import SwiftParserDiagnostics
import SwiftSyntax

/// Describes a source file that has been "exported" to the C++ part of the
/// compiler, with enough information to interface with the C++ layer.
public struct ExportedSourceFile {
  /// The underlying buffer within the C++ SourceManager, which is used
  /// for computations of source locations.
  public let buffer: UnsafeBufferPointer<UInt8>

  /// The name of the enclosing module.
  let moduleName: String

  /// The name of the source file being parsed.
  let fileName: String

  /// The syntax tree for the complete source file.
  public let syntax: SourceFileSyntax

  /// A source location converter to convert `AbsolutePosition`s in `syntax` to line/column locations.
  ///
  /// Cached so we don't need to re-build the line table every time we need to convert a position.
  let sourceLocationConverter: SourceLocationConverter

  /// Configured regions for this source file.
  ///
  /// This is a cached value; access via configuredRegions(astContext:).
  var _configuredRegions: ConfiguredRegions? = nil

  public func position(of location: BridgedSourceLoc) -> AbsolutePosition? {
    let sourceFileBaseAddress = UnsafeRawPointer(buffer.baseAddress!)
    guard let opaqueValue = location.getOpaquePointerValue() else {
      return nil
    }
    return AbsolutePosition(utf8Offset: opaqueValue - sourceFileBaseAddress)
  }

  /// Retrieve a bridged source location for the given absolute position in
  /// this source file.
  public func sourceLoc(at position: AbsolutePosition) -> BridgedSourceLoc {
    BridgedSourceLoc(at: position, in: buffer)
  }
}

extension Parser.ExperimentalFeatures {
  init(from context: BridgedASTContext?) {
    self = []
    guard let context = context else { return }

    func mapFeature(_ bridged: BridgedFeature, to feature: Self) {
      if context.langOptsHasFeature(bridged) {
        insert(feature)
      }
    }

    mapFeature(.ThenStatements, to: .thenStatements)
    mapFeature(.DoExpressions, to: .doExpressions)
    mapFeature(.NonescapableTypes, to: .nonescapableTypes)
    mapFeature(.TrailingComma, to: .trailingComma)
  }
}

extension Parser.SwiftVersion {
  init?(from context: BridgedASTContext?) {
    guard let context else {
      return nil
    }
    switch context.majorLanguageVersion {
    case 1, 2, 3, 4: self = .v4
    case 5: self = .v5
    case 6: self = .v6
    default: self = .v6
    }
  }
}

/// Parses the given source file and produces a pointer to a new
/// ExportedSourceFile instance.
@_cdecl("swift_ASTGen_parseSourceFile")
public func parseSourceFile(
  buffer: UnsafePointer<UInt8>,
  bufferLength: Int,
  moduleName: UnsafePointer<UInt8>,
  filename: UnsafePointer<UInt8>,
  ctxPtr: UnsafeMutableRawPointer?
) -> UnsafeRawPointer {
  let buffer = UnsafeBufferPointer(start: buffer, count: bufferLength)

  let ctx = ctxPtr.map { BridgedASTContext(raw: $0) }
  let sourceFile = Parser.parse(
    source: buffer,
    swiftVersion: Parser.SwiftVersion(from: ctx),
    experimentalFeatures: .init(from: ctx)
  )

  let exportedPtr = UnsafeMutablePointer<ExportedSourceFile>.allocate(capacity: 1)
  let moduleName = String(cString: moduleName)
  let fileName = String(cString: filename)
  exportedPtr.initialize(
    to: .init(
      buffer: buffer,
      moduleName: moduleName,
      fileName: fileName,
      syntax: sourceFile,
      sourceLocationConverter: SourceLocationConverter(fileName: fileName, tree: sourceFile)
    )
  )

  return UnsafeRawPointer(exportedPtr)
}

/// Deallocate a parsed source file.
@_cdecl("swift_ASTGen_destroySourceFile")
public func destroySourceFile(
  sourceFilePtr: UnsafeMutablePointer<UInt8>
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sourceFile in
    sourceFile.deinitialize(count: 1)
    sourceFile.deallocate()
  }
}

/// Check for whether the given source file round-trips
@_cdecl("swift_ASTGen_roundTripCheck")
public func roundTripCheck(
  sourceFilePtr: UnsafeMutablePointer<UInt8>
) -> CInt {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sourceFile in
    let sf = sourceFile.pointee
    return sf.syntax.syntaxTextBytes.elementsEqual(sf.buffer) ? 0 : 1
  }
}

/// Emit diagnostics within the given source file.
@_cdecl("swift_ASTGen_emitParserDiagnostics")
public func emitParserDiagnostics(
  ctx: BridgedASTContext,
  diagEnginePtr: UnsafeMutableRawPointer,
  sourceFilePtr: UnsafeMutablePointer<UInt8>,
  emitOnlyErrors: CInt,
  downgradePlaceholderErrorsToWarnings: CInt
) -> CInt {
  return sourceFilePtr.withMemoryRebound(
    to: ExportedSourceFile.self,
    capacity: 1
  ) { sourceFile in
    var anyDiags = false

    let sourceFileSyntax = sourceFile.pointee.syntax
    let diags = ParseDiagnosticsGenerator.diagnostics(for: sourceFileSyntax)

    let diagnosticEngine = BridgedDiagnosticEngine(raw: diagEnginePtr)
    let configuredRegions = sourceFile.pointee.configuredRegions(astContext: ctx)
    for diag in diags {
      // If the diagnostic is in an unparsed #if region, don't emit it.
      if configuredRegions.isActive(diag.node) == .unparsed {
        continue
      }

      let diagnosticSeverity: DiagnosticSeverity
      if downgradePlaceholderErrorsToWarnings == 1
        && diag.diagMessage.diagnosticID == StaticTokenError.editorPlaceholder.diagnosticID
      {
        diagnosticSeverity = .warning
      } else {
        diagnosticSeverity = diag.diagMessage.severity
      }

      if emitOnlyErrors != 0, diagnosticSeverity != .error {
        continue
      }

      emitDiagnostic(
        diagnosticEngine: diagnosticEngine,
        sourceFileBuffer: sourceFile.pointee.buffer,
        diagnostic: diag,
        diagnosticSeverity: diagnosticSeverity
      )
      anyDiags = true
    }

    return anyDiags ? 1 : 0
  }
}
