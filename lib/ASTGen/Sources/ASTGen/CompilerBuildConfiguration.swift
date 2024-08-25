//===--- CompilerBuildConfiguration.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import SwiftDiagnostics
@_spi(Compiler) import SwiftIfConfig
import SwiftParser
import SwiftSyntax

/// A build configuration that uses the compiler's ASTContext to answer
/// queries.
struct CompilerBuildConfiguration: BuildConfiguration {
  let ctx: BridgedASTContext
  let sourceBuffer: UnsafeBufferPointer<UInt8>

  init(ctx: BridgedASTContext, sourceBuffer: UnsafeBufferPointer<UInt8>) {
    self.ctx = ctx
    self.sourceBuffer = sourceBuffer
  }

  func isCustomConditionSet(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsCustomConditionSet(nameRef)
    }
  }
  
  func hasFeature(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsHasFeatureNamed(nameRef)
    }
  }
  
  func hasAttribute(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsHasAttributeNamed(nameRef)
    }
  }
  
  func canImport(
    importPath: [(TokenSyntax, String)],
    version: CanImportVersion
  ) throws -> Bool {
    var importPathStr = importPath.map { $0.1 }.joined(separator: ".")

    var versionComponents: [Int]
    let cVersionKind: BridgedCanImportVersion
    switch version {
    case .unversioned:
      cVersionKind = .CanImportUnversioned
      versionComponents = []

    case .version(let versionTuple):
      cVersionKind = .CanImportVersion
      versionComponents = versionTuple.components

    case .underlyingVersion(let versionTuple):
      cVersionKind = .CanImportUnderlyingVersion
      versionComponents = versionTuple.components
    }

    return importPathStr.withBridgedString { bridgedImportPathStr in
      versionComponents.withUnsafeBufferPointer { versionComponentsBuf in
        ctx.canImport(
          importPath: bridgedImportPathStr,
          location: BridgedSourceLoc(
            at: importPath.first!.0.position,
            in: sourceBuffer
          ),
          versionKind: cVersionKind,
          versionComponents: versionComponentsBuf.baseAddress,
          numVersionComponents: versionComponentsBuf.count
        )
      }
    }
  }
  
  func isActiveTargetOS(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetOS(nameRef)
    }
  }
  
  func isActiveTargetArchitecture(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetArchitecture(nameRef)
    }
  }
  
  func isActiveTargetEnvironment(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetEnvironment(nameRef)
    }
  }
  
  func isActiveTargetRuntime(name: String) throws -> Bool {
    var name = name

    // Complain if the provided runtime isn't one of the known values.
    switch name {
    case "_Native", "_ObjC", "_multithreaded": break
    default: throw IfConfigError.unexpectedRuntimeCondition
    }

    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetRuntime(nameRef)
    }
  }
  
  func isActiveTargetPointerAuthentication(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetPtrAuth(nameRef)
    }
  }
  
  var targetPointerBitWidth: Int {
    Int(ctx.langOptsTargetPointerBitWidth)
  }

  var targetAtomicBitWidths: [Int] {
    var bitWidthsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetTargetAtomicBitWidths(&bitWidthsBuf)
    let bitWidths = Array(UnsafeMutableBufferPointer(start: bitWidthsBuf, count: count))
    deallocateIntBuffer(bitWidthsBuf);
    return bitWidths
  }

  var endianness: Endianness {
    switch ctx.langOptsTargetEndianness {
    case .EndianBig: return .big
    case .EndianLittle: return .little
    }
  }

  var languageVersion: VersionTuple { 
    var componentsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetLanguageVersion(&componentsBuf)
    let version = VersionTuple(
      components: Array(UnsafeMutableBufferPointer(start: componentsBuf, count: count))
    )
    deallocateIntBuffer(componentsBuf);
    return version
  }

  var compilerVersion: VersionTuple { 
    var componentsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetCompilerVersion(&componentsBuf)
    let version = VersionTuple(
      components: Array(UnsafeMutableBufferPointer(start: componentsBuf, count: count))
    )
    deallocateIntBuffer(componentsBuf);
    return version
  }
}

enum IfConfigError: Error, CustomStringConvertible {
  case unexpectedRuntimeCondition

  var description: String {
    switch self {
      case .unexpectedRuntimeCondition:
        return "unexpected argument for the '_runtime' condition; expected '_Native' or '_ObjC'"
    }
  }
}


/// Extract the #if clause range information for the given source file.
@_cdecl("swift_ASTGen_configuredRegions")
public func configuredRegions(
  astContext: BridgedASTContext,
  sourceFilePtr: UnsafeRawPointer,
  cRegionsOut: UnsafeMutablePointer<UnsafeMutablePointer<BridgedIfConfigClauseRangeInfo>?>
) -> Int {
  let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
  let configuration = CompilerBuildConfiguration(
    ctx: astContext,
    sourceBuffer: sourceFilePtr.pointee.buffer
  )
  let regions = sourceFilePtr.pointee.syntax.configuredRegions(in: configuration)

  var cRegions: [BridgedIfConfigClauseRangeInfo] = []

  // Keep track of the enclosing #ifs so that we can emit an "#endif" directive
  // right before moving on to the next #if (and at the end).
  var ifConfigStack: [IfConfigDeclSyntax] = []

  /// Emit the #endif location for the given #if declaration.
  func flushSingleIfConfig(_ topIfConfigDecl: IfConfigDeclSyntax) {
    cRegions.append(
      .init(
        directiveLoc: sourceFilePtr.pointee.sourceLoc(
          at: topIfConfigDecl.poundEndif.positionAfterSkippingLeadingTrivia
        ),
        bodyLoc: sourceFilePtr.pointee.sourceLoc(
          at: topIfConfigDecl.poundEndif.endPosition
        ),
        endLoc: sourceFilePtr.pointee.sourceLoc(
          at: topIfConfigDecl.poundEndif.endPosition
        ),
        kind: .IfConfigEnd
      )
    )
  }

  /// Push a new #if declaration into the stack so that we'll insert #endifs
  /// in the right places.
  func pushIfConfig(_ currentIfConfigDecl: IfConfigDeclSyntax) {
    // Go through the current stack of #if declarations.
    while let topIfConfig = ifConfigStack.last {
      // If the top of the stack is the same as this #if, we're done.
      if topIfConfig == currentIfConfigDecl {
        return
      }

      // If the top of the stack is not an ancestor of this #if, flush it
      // and keep going.
      if !topIfConfig.isAncestor(of: currentIfConfigDecl) {
        flushSingleIfConfig(topIfConfig)
        ifConfigStack.removeLast()
        continue
      }

      break
    }

    // Add this #if to the stack.
    ifConfigStack.append(currentIfConfigDecl)
  }

  // Translate all of the configured regions.
  for (ifConfig, state) in regions {
    // Note that we're handling an #if now.
    if let currentIfConfigDecl = ifConfig.parent?.parent?.as(IfConfigDeclSyntax.self) {
      pushIfConfig(currentIfConfigDecl)
    }

    let kind: BridgedIfConfigClauseKind
    switch state {
    case .active: kind = .IfConfigActive
    case .inactive, .unparsed: kind = .IfConfigInactive
    }

    let bodyLoc: AbsolutePosition
    if let elements = ifConfig.elements {
      bodyLoc = elements.position
    } else if let condition = ifConfig.condition {
      bodyLoc = condition.endPosition
    } else {
      bodyLoc = ifConfig.endPosition
    }

    let endLoc: AbsolutePosition
    if let nextToken = ifConfig.nextToken(viewMode: .sourceAccurate) {
      endLoc = nextToken.positionAfterSkippingLeadingTrivia
    } else {
      endLoc = ifConfig.endPosition
    }

    cRegions.append(
      .init(
        directiveLoc: sourceFilePtr.pointee.sourceLoc(
          at: ifConfig.poundKeyword.positionAfterSkippingLeadingTrivia
        ),
        bodyLoc: sourceFilePtr.pointee.sourceLoc(at: bodyLoc),
        endLoc: sourceFilePtr.pointee.sourceLoc(at: endLoc),
        kind: kind
      )
    )
  }

  // Flush the remaining #ifs.
  while let topIfConfig = ifConfigStack.popLast() {
    flushSingleIfConfig(topIfConfig)
  }

  let cRegionsBuf: UnsafeMutableBufferPointer<BridgedIfConfigClauseRangeInfo> =
    .allocate(capacity: cRegions.count)
  _ = cRegionsBuf.initialize(from: cRegions)
  cRegionsOut.pointee = cRegionsBuf.baseAddress
  return cRegionsBuf.count
}

extension SyntaxProtocol {
  /// Determine whether this node is an ancestor of the given `other` node.
  func isAncestor(of other: some SyntaxProtocol) -> Bool {
    var other = Syntax(other)
    let selfSyntax = Syntax(self)
    while let otherParent = other.parent {
      if otherParent == selfSyntax {
        return true
      }

      other = otherParent
    }

    return false
  }
}

@_cdecl("swift_ASTGen_freeConfiguredRegions")
public func freeConfiguredRegions(
  regions: UnsafeMutablePointer<BridgedIfConfigClauseRangeInfo>?,
  numRegions: Int
) {
  UnsafeMutableBufferPointer(start: regions, count: numRegions).deallocate()
}

/// Evaluate the #if condition at ifClauseLocationPtr.
@_cdecl("swift_ASTGen_evaluatePoundIfCondition")
public func evaluatePoundIfCondition(
  astContext: BridgedASTContext,
  diagEnginePtr: UnsafeMutableRawPointer,
  sourceFileBuffer: BridgedStringRef,
  ifConditionText: BridgedStringRef,
  shouldEvaluate: Bool
) -> Int {
  // Retrieve the #if condition that we're evaluating here.
  // FIXME: Use 'ExportedSourceFile' when C++ parser is replaced.
  let textBuffer = UnsafeBufferPointer<UInt8>(start: ifConditionText.data, count: ifConditionText.count)
  var parser = Parser(textBuffer)
  let conditionExpr = ExprSyntax.parse(from: &parser)

  let isActive: Bool
  let syntaxErrorsAllowed: Bool
  let diagnostics: [Diagnostic]
  if shouldEvaluate {
    // Evaluate the condition against the compiler's build configuration.
    let configuration = CompilerBuildConfiguration(
      ctx: astContext,
      sourceBuffer: textBuffer
    )

    let state: IfConfigRegionState
    (state, syntaxErrorsAllowed, diagnostics) = IfConfigRegionState.evaluating(conditionExpr, in: configuration)
    isActive = (state == .active)
  } else {
    // Don't evaluate the condition, because we know it's inactive. Determine
    // whether syntax errors are permitted within this region according to the
    // condition.
    isActive = false
    (syntaxErrorsAllowed, diagnostics) = IfConfigClauseSyntax.syntaxErrorsAllowed(conditionExpr)
  }

  // Render the diagnostics.
  for diagnostic in diagnostics {
    emitDiagnostic(
      diagnosticEngine: BridgedDiagnosticEngine(raw: diagEnginePtr),
      sourceFileBuffer: UnsafeBufferPointer(start: sourceFileBuffer.data, count: sourceFileBuffer.count),
      sourceFileBufferOffset: ifConditionText.data! - sourceFileBuffer.data!,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }

  return (isActive ? 0x1 : 0) | (syntaxErrorsAllowed ? 0x2 : 0)
}
