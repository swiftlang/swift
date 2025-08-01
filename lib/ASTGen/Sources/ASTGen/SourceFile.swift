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
@_spi(Compiler) import SwiftSyntax

/// Describes a source file that has been "exported" to the C++ part of the
/// compiler, with enough information to interface with the C++ layer.
public struct ExportedSourceFile {
  /// The underlying buffer within the C++ SourceManager, which is used
  /// for computations of source locations.
  public let buffer: UnsafeBufferPointer<UInt8>

  /// The name of the enclosing module.
  public let moduleName: String

  /// The name of the source file being parsed.
  public let fileName: String

  /// The syntax tree for the complete source file.
  public let syntax: Syntax

  /// A source location converter to convert `AbsolutePosition`s in `syntax` to line/column locations.
  ///
  /// Cached so we don't need to re-build the line table every time we need to convert a position.
  public let sourceLocationConverter: SourceLocationConverter

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

    mapFeature(.ReferenceBindings, to: .referenceBindings)
    mapFeature(.ThenStatements, to: .thenStatements)
    mapFeature(.DoExpressions, to: .doExpressions)
    mapFeature(.NonescapableTypes, to: .nonescapableTypes)
    mapFeature(.TrailingComma, to: .trailingComma)
    mapFeature(.CoroutineAccessors, to: .coroutineAccessors)
    mapFeature(.OldOwnershipOperatorSpellings, to: .oldOwnershipOperatorSpellings)
    mapFeature(.KeyPathWithMethodMembers, to: .keypathWithMethodMembers)
    mapFeature(.DefaultIsolationPerFile, to: .defaultIsolationPerFile)
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
  buffer: BridgedStringRef,
  moduleName: BridgedStringRef,
  filename: BridgedStringRef,
  declContextPtr: UnsafeMutableRawPointer?,
  kind: BridgedGeneratedSourceFileKind
) -> UnsafeRawPointer {
  let buffer = UnsafeBufferPointer(start: buffer.data, count: buffer.count)
  let dc = declContextPtr.map { BridgedDeclContext(raw: $0) }
  let ctx = dc?.astContext

  var parser = Parser(
    buffer,
    swiftVersion: Parser.SwiftVersion(from: ctx),
    experimentalFeatures: Parser.ExperimentalFeatures(from: ctx)
  )

  let parsed: Syntax
  switch kind {
  case .none, // Top level source file.
      .expressionMacroExpansion,
      .conformanceMacroExpansion,
      .extensionMacroExpansion,
      .preambleMacroExpansion,
      .replacedFunctionBody,
      .prettyPrinted,
      .defaultArgument:
    parsed = Syntax(SourceFileSyntax.parse(from: &parser))

  case .declarationMacroExpansion,
      .codeItemMacroExpansion,
      .peerMacroExpansion:
    if let dc, dc.isTypeContext {
      parsed = Syntax(MemberBlockItemListFileSyntax.parse(from: &parser))
    } else {
      parsed = Syntax(SourceFileSyntax.parse(from: &parser))
    }

  case .memberMacroExpansion:
    parsed = Syntax(MemberBlockItemListFileSyntax.parse(from: &parser))

  case .accessorMacroExpansion:
    parsed = Syntax(AccessorBlockFileSyntax.parse(from: &parser))

  case .memberAttributeMacroExpansion:
    var attrs = AttributeClauseFileSyntax.parse(from: &parser)
    if !attrs.modifiers.isEmpty {
      // 'memberAttribute' macro doesn't allow modifiers. Move to "unexpected" if any.
      attrs.unexpectedBetweenAttributesAndModifiers = [Syntax(attrs.modifiers)]
      attrs.modifiers = []
    }
    parsed = Syntax(attrs)

  case .attributeFromClang:
    parsed = Syntax(AttributeClauseFileSyntax.parse(from: &parser))

  case .bodyMacroExpansion:
    parsed = Syntax(CodeBlockFileSyntax.parse(from: &parser))
  }

  let exportedPtr = UnsafeMutablePointer<ExportedSourceFile>.allocate(capacity: 1)
  let moduleName = String(bridged: moduleName)
  let fileName = String(bridged: filename)
  exportedPtr.initialize(
    to: .init(
      buffer: buffer,
      moduleName: moduleName,
      fileName: fileName,
      syntax: parsed,
      sourceLocationConverter: SourceLocationConverter(fileName: fileName, tree: parsed)
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

/// Find a token in the given source file at the given location.
func findToken(
  in sourceFilePtr: UnsafeRawPointer,
  at sourceLocationPtr: UnsafePointer<UInt8>?
) -> TokenSyntax? {
  guard let sourceLocationPtr = sourceLocationPtr else {
    return nil
  }

  let sourceFilePtr = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)

  // Find the offset.
  let buffer = sourceFilePtr.pointee.buffer
  let offset = sourceLocationPtr - buffer.baseAddress!
  if offset < 0 || offset >= buffer.count {
    print("source location isn't inside this buffer")
    return nil
  }

  // Find the token at that offset.
  let sf = sourceFilePtr.pointee.syntax
  guard let token = sf.token(at: AbsolutePosition(utf8Offset: offset)) else {
    print("couldn't find token at offset \(offset)")
    return nil
  }

  return token
}

/// Retrieve a syntax node in the given source file, with the given type.
public func findSyntaxNodeInSourceFile<Node: SyntaxProtocol>(
  sourceFilePtr: UnsafeRawPointer,
  sourceLocationPtr: UnsafePointer<UInt8>?,
  type: Node.Type,
  wantOutermost: Bool = false
) -> Node? {
  guard let token = findToken(in: sourceFilePtr, at: sourceLocationPtr) else {
    return nil
  }

  var currentSyntax = Syntax(token)
  var resultSyntax: Node? = nil
  while let parentSyntax = currentSyntax.parent {
    currentSyntax = parentSyntax
    if let typedParent = currentSyntax.as(type) {
      resultSyntax = typedParent
      break
    }
  }

  // If we didn't find anything, return nil.
  guard var resultSyntax else {
    return nil
  }

  // If we want the outermost node, keep looking.
  // E.g. for 'foo.bar' we want the member ref expression instead of the
  // identifier expression.
  if wantOutermost {
    while let parentSyntax = currentSyntax.parent,
      parentSyntax.position == resultSyntax.position
    {
      currentSyntax = parentSyntax
      if let typedParent = currentSyntax.as(type) {
        resultSyntax = typedParent
      }
    }
  }

  return resultSyntax
}

/// Retrieve a syntax node in the given source file that satisfies the
/// given predicate.
public func findSyntaxNodeInSourceFile(
  sourceFilePtr: UnsafeRawPointer,
  sourceLocationPtr: UnsafePointer<UInt8>?,
  where predicate: (Syntax) -> Bool
) -> Syntax? {
  guard let token = findToken(in: sourceFilePtr, at: sourceLocationPtr) else {
    return nil
  }

  var currentSyntax = Syntax(token)
  while let parentSyntax = currentSyntax.parent {
    currentSyntax = parentSyntax
    if predicate(currentSyntax) {
      return currentSyntax
    }
  }

  return nil
}

@_cdecl("swift_ASTGen_virtualFiles")
@usableFromInline
func getVirtualFiles(
  sourceFilePtr: UnsafeMutableRawPointer,
  cVirtualFilesOut: UnsafeMutablePointer<UnsafeMutablePointer<BridgedVirtualFile>?>
) -> Int {
  let sourceFilePtr = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let virtualFiles = sourceFilePtr.pointee.sourceLocationConverter.lineTable.virtualFiles
  guard !virtualFiles.isEmpty else {
    cVirtualFilesOut.pointee = nil
    return 0
  }

  let cArrayBuf: UnsafeMutableBufferPointer<BridgedVirtualFile> = .allocate(capacity: virtualFiles.count)
  _ = cArrayBuf.initialize(
    from: virtualFiles.lazy.map({ virtualFile in
      BridgedVirtualFile(
        StartPosition: virtualFile.startPosition.utf8Offset,
        EndPosition: virtualFile.endPosition.utf8Offset,
        Name: allocateBridgedString(virtualFile.fileName),
        LineOffset: virtualFile.lineOffset,
        NamePosition: virtualFile.fileNamePosition.utf8Offset
      )
    })
  )

  cVirtualFilesOut.pointee = cArrayBuf.baseAddress
  return cArrayBuf.count
}

@_cdecl("swift_ASTGen_freeBridgedVirtualFiles")
func freeVirtualFiles(
  cVirtualFiles: UnsafeMutablePointer<BridgedVirtualFile>?,
  numFiles: Int
) {
  let buffer = UnsafeMutableBufferPointer<BridgedVirtualFile>(start: cVirtualFiles, count: numFiles)
  for vFile in buffer {
    freeBridgedString(bridged: vFile.Name)
  }
  buffer.deinitialize()
  buffer.deallocate()
}
