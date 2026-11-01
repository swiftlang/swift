//===--- DelayedBodies.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Function bodies are built on demand. While generating a declaration, ASTGen
// records only the source range of the body of a function, initializer,
// deinitializer or accessor; the C++ `ParseAbstractFunctionBodyRequest` calls
// back into `swift_ASTGen_buildFunctionBody` when something needs the body.
// That is what lets the type checker skip bodies without ever building them,
// and it means syntax errors in a body are only reported if the body is.

import ASTBridging
import SwiftDiagnostics
import SwiftIfConfig
import SwiftParserDiagnostics
@_spi(RawSyntax) @_spi(Compiler) import SwiftSyntax

/// Whether the body of the given function-like declaration is eligible for
/// being built on demand. This depends only on the syntax, because it has to
/// give the same answer when diagnostics are emitted, when the declaration is
/// generated, and when the body is built.
func isDelayableFunctionBody(_ body: CodeBlockSyntax) -> Bool {
  guard body.leftBrace.presence == .present, body.rightBrace.presence == .present,
    let decl = body.parent
  else {
    return false
  }

  switch decl.kind {
  case .functionDecl, .initializerDecl, .deinitializerDecl, .accessorDecl:
    break
  default:
    return false
  }

  return isDelayableContext(startingAt: decl.parent) && !containsSourceLocationDirective(body)
}

/// Whether the body of a computed property or subscript written as just its
/// getter's statements, `var x: Int { ... }`, is eligible for being built on
/// demand.
func isDelayableImplicitGetterBody(_ accessorBlock: AccessorBlockSyntax) -> Bool {
  guard case .getter = accessorBlock.accessors,
    accessorBlock.leftBrace.presence == .present,
    accessorBlock.rightBrace.presence == .present
  else {
    return false
  }
  return isDelayableContext(startingAt: accessorBlock.parent) && !containsSourceLocationDirective(accessorBlock)
}

/// Whether a declaration whose parent is \p start is outside any other body.
private func isDelayableContext(startingAt start: Syntax?) -> Bool {
  // A declaration inside another body is built along with that body.
  var current = start
  while let node = current {
    switch node.kind {
    case .sourceFile:
      return true
    case .memberBlockItemList, .memberBlockItem, .memberBlock,
      .structDecl, .classDecl, .enumDecl, .actorDecl, .extensionDecl,
      .codeBlockItem, .ifConfigDecl, .ifConfigClauseList, .ifConfigClause,
      .accessorDeclList, .accessorBlock, .patternBindingList, .patternBinding,
      .variableDecl, .subscriptDecl:
      break
    case .codeBlockItemList:
      // Only the top level of a file or of an `#if` clause.
      guard let parent = node.parent,
        parent.kind == .sourceFile || parent.kind == .ifConfigClause
      else {
        return false
      }
    default:
      return false
    }
    current = node.parent
  }
  return false
}

/// A `#sourceLocation` directive changes how locations map, and its effect
/// cannot be separated from the code around it. A body with one, or with a
/// regex literal, is built right away.
private func containsSourceLocationDirective(_ body: some SyntaxProtocol) -> Bool {
  var previousWasPound = false
  for token in body.tokens(viewMode: .sourceAccurate) {
    switch token.tokenKind {
    case .poundSourceLocation, .regexSlash, .regexPoundDelimiter:
      // Regex literals are never skipped either: whether a `/` starts one
      // depends on what follows.
      return true
    default:
      break
    }
    if previousWasPound, token.text == "line" {
      return true
    }
    previousWasPound = token.tokenKind == .pound
  }
  return false
}

/// Whether the body declares a type, which the C++ parser records because such
/// a body cannot always be skipped.
private func hasNestedTypeDeclarations(_ body: some SyntaxProtocol) -> Bool {
  for token in body.tokens(viewMode: .sourceAccurate) {
    switch token.tokenKind {
    case .keyword(.class), .keyword(.struct), .keyword(.enum),
      .keyword(.typealias), .keyword(.protocol):
      return true
    case .keyword(.actor), .identifier("actor"):
      return true
    default:
      break
    }
  }
  return false
}

/// Finds the bodies that would be delayed, outermost first, without looking
/// inside them.
private final class DelayedBodyCollector: SyntaxVisitor {
  var ranges: [Range<Int>] = []

  override func visit(_ node: CodeBlockSyntax) -> SyntaxVisitorContinueKind {
    if isDelayableFunctionBody(node) {
      ranges.append(
        node.positionAfterSkippingLeadingTrivia.utf8Offset..<node.endPositionBeforeTrailingTrivia.utf8Offset
      )
      return .skipChildren
    }
    return .visitChildren
  }

  override func visit(_ node: AccessorBlockSyntax) -> SyntaxVisitorContinueKind {
    if isDelayableImplicitGetterBody(node) {
      ranges.append(
        node.positionAfterSkippingLeadingTrivia.utf8Offset..<node.endPositionBeforeTrailingTrivia.utf8Offset
      )
      return .skipChildren
    }
    return .visitChildren
  }
}

extension ExportedSourceFile {
  /// The ranges of the function bodies ASTGen builds on demand.
  func delayedBodyRanges() -> [Range<Int>] {
    let collector = DelayedBodyCollector(viewMode: .sourceAccurate)
    collector.walk(self.syntax)
    return collector.ranges
  }
}

/// Splits \p diagnostics into those that lie inside a delayed body, keyed by
/// the offset of the body's `{`, and the rest.
func partitionDiagnostics(
  _ diagnostics: [Diagnostic],
  byDelayedBodies ranges: [Range<Int>]
) -> (outside: [Diagnostic], inside: [Int: [Diagnostic]]) {
  var outside: [Diagnostic] = []
  var inside: [Int: [Diagnostic]] = [:]
  for diagnostic in diagnostics {
    let offset = diagnostic.node.positionAfterSkippingLeadingTrivia.utf8Offset
    // `ranges` is in source order.
    var low = 0
    var high = ranges.count
    while low < high {
      let mid = (low + high) / 2
      if ranges[mid].upperBound <= offset {
        low = mid + 1
      } else {
        high = mid
      }
    }
    if low < ranges.count, ranges[low].contains(offset) {
      inside[ranges[low].lowerBound, default: []].append(diagnostic)
    } else {
      outside.append(diagnostic)
    }
  }
  return (outside, inside)
}

extension ASTGenVisitor {
  /// Attach the body of \p decl: either build it now or, when it is eligible,
  /// only record where it is.
  func attachBody(
    _ body: CodeBlockSyntax,
    to decl: BridgedDecl,
    declContext: BridgedDeclContext,
    setParsed: (BridgedBraceStmt) -> Void
  ) {
    if self.delayFunctionBodies, isDelayableFunctionBody(body) {
      decl.setBodyDelayed(
        range: SourceRange(
          start: self.generateSourceLoc(body.leftBrace),
          end: self.generateSourceLoc(body.rightBrace)
        ),
        hasNestedTypeDeclarations: hasNestedTypeDeclarations(body)
      )
      return
    }
    // The type checker needs to know whether the body declares types even when
    // it is not delayed.
    decl.setHasNestedTypeDeclarations(hasNestedTypeDeclarations(body))
    self.withDeclContext(declContext) {
      setParsed(self.generate(codeBlock: body))
    }
  }
}

extension ASTGenVisitor {
  /// Attach the body of the getter written as just statements in \p block.
  func attachImplicitGetterBody(
    _ block: AccessorBlockSyntax,
    items: CodeBlockItemListSyntax,
    leftBrace: SourceLoc,
    rightBrace: SourceLoc,
    to accessor: BridgedAccessorDecl
  ) {
    if self.delayFunctionBodies, isDelayableImplicitGetterBody(block) {
      accessor.asDecl.setBodyDelayed(
        range: SourceRange(
          start: self.generateSourceLoc(block.leftBrace),
          end: self.generateSourceLoc(block.rightBrace)
        ),
        hasNestedTypeDeclarations: hasNestedTypeDeclarations(block)
      )
      return
    }
    accessor.asDecl.setHasNestedTypeDeclarations(hasNestedTypeDeclarations(block))
    self.withDeclContext(accessor.asDeclContext) {
      accessor.setParsedBody(
        self.generateImplicitGetterBody(items, leftBrace: leftBrace, rightBrace: rightBrace))
    }
  }

  func generateImplicitGetterBody(
    _ items: CodeBlockItemListSyntax,
    leftBrace: SourceLoc,
    rightBrace: SourceLoc
  ) -> BridgedBraceStmt {
    BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: leftBrace,
      elements: self.generate(codeBlockItemList: items).lazy.bridgedArray(in: self),
      rBraceLoc: rightBrace
    )
  }
}

/// Build the body of a function-like declaration whose body was delayed.
@_cdecl("swift_ASTGen_buildFunctionBody")
public func buildFunctionBody(
  diagEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeMutableRawPointer,
  decl: BridgedDecl,
  declContext: BridgedDeclContext,
  ctx: BridgedASTContext,
  bodyStart: SourceLoc
) {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  guard let position = sourceFile.pointee.position(of: bodyStart),
    let token = sourceFile.pointee.syntax.token(at: position),
    let bodyNode = token.parent
  else {
    preconditionFailure("delayed function body not found in the syntax tree")
  }
  let offset = bodyNode.positionAfterSkippingLeadingTrivia.utf8Offset

  let visitor = ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: declContext,
    astContext: ctx,
    configuredRegions: sourceFile.pointee.configuredRegions(astContext: ctx)
  )

  // The diagnostics that were held back for this body come first, as they
  // would have in the source.
  let held = sourceFile.pointee.heldBackDiagnostics(astContext: ctx)
  if let parserDiagnostics = held.parser[offset] {
    for diagnostic in parserDiagnostics {
      let severity = diagnostic.diagMessage.severity
      var emitted = severity
      if sourceFile.pointee.downgradePlaceholderErrorsToWarnings,
        diagnostic.diagMessage.diagnosticID == StaticTokenError.editorPlaceholder.diagnosticID
      {
        emitted = .warning
      }
      emitDiagnostic(
        diagnosticEngine: diagEngine,
        sourceFileBuffer: sourceFile.pointee.buffer,
        diagnostic: diagnostic,
        diagnosticSeverity: emitted
      )
    }
  }

  if let body = bodyNode.as(CodeBlockSyntax.self) {
    decl.setDelayedBodyParsed(visitor.generate(codeBlock: body))
  } else if let block = bodyNode.as(AccessorBlockSyntax.self),
    case .getter(let items) = block.accessors
  {
    decl.setDelayedBodyParsed(
      visitor.generateImplicitGetterBody(
        items,
        leftBrace: visitor.generateSourceLoc(block.leftBrace),
        rightBrace: visitor.generateSourceLoc(block.rightBrace)))
  } else {
    preconditionFailure("delayed function body is not a code block")
  }

  if let configDiagnostics = held.ifConfig[offset] {
    visitor.diagnoseAll(configDiagnostics)
  }
}
