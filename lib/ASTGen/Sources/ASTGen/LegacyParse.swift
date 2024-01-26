//===--- LegacyParse.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import ParseBridging
import SwiftSyntax

extension ASTGenVisitor {

  func generateWithLegacy(_ node: ExprSyntax) -> BridgedExpr {
    // NOTE: Postfix expressions share the same start location with the inner
    // expression. This function must only be called on the outermost expression
    // that shares the same position. See also `isExprMigrated(_:)`

    // FIXME: Calculate isExprBasic.
    let isExprBasic = false
    return legacyParse.parseExpr(self.generateSourceLoc(node), self.declContext, isExprBasic)
  }

  func generateWithLegacy(_ node: DeclSyntax) -> BridgedDecl {
    legacyParse.parseDecl(self.generateSourceLoc(node), self.declContext)
  }

  func generateWithLegacy(_ node: StmtSyntax) -> BridgedStmt {
    legacyParse.parseStmt(self.generateSourceLoc(node), self.declContext)
  }

  /// Parse a `TypeRepr` at the given `TypeSyntax` node with the legacy parser.
  ///
  /// - Parameter generateChildrenWithASTGen: If `true`, the legacy parser will
  ///   recurse back into ASTGen to generate child nodes.
  func generateWithLegacy(_ node: TypeSyntax, generateChildrenWithASTGen: Bool) -> BridgedTypeRepr {
    return legacyParse.parseType(
      loc: self.generateSourceLoc(node),
      declContext: self.declContext,
      generateChildrenWithASTGen: generateChildrenWithASTGen
    )
  }

  func generateMatchingPatternWithLegacy(_ node: some PatternSyntaxProtocol) {
    //    legacyParse.parseMatchingPattern(self.bridgedSourceLoc(syntax: node), self.declContext)
  }

  func generateBindingPatternWithLegacy(_ node: some PatternSyntaxProtocol) {
    //    legacyParse.parseBindingPattern(self.bridgedSourceLoc(syntax: node), self.declContext)
  }
}

extension ASTGenVisitor {
  /// Validate the result of translating a given `TypeSyntax` against the legacy
  /// parser and emit errors on unexpected mismatches.
  ///
  /// - Parameters:
  ///   - astgenResult The translation result.
  ///   - source The translation source.
  func validateGeneratedTypeRepr(_ astgenResult: BridgedTypeRepr, from source: TypeSyntax) {
    // We are using the legacy parser here just for validation; suppress
    // diagnostics.
    self.diagnosticSuppression.start()
    let legacyParserResult = self.generateWithLegacy(source, generateChildrenWithASTGen: false)
    self.diagnosticSuppression.stop()

    ParseBridging.validateGeneratedTypeRepr(
      self.ctx,
      legacyParserResult: legacyParserResult,
      astgenResult: astgenResult
    )
  }
}

@_cdecl("swift_ASTGen_validateTypeReprGeneration")
public func validateTypeReprGeneration(
  diagnosticEngine: BridgedDiagnosticEngine,
  sourceFilePtr: UnsafeRawPointer,
  declContext: BridgedDeclContext,
  astContext: BridgedASTContext,
  legacyParser: BridgedLegacyParser
) {
  class TypeVisitor: SwiftSyntax.SyntaxAnyVisitor {
    let astgen: ASTGenVisitor
    var suppression: BridgedDiagnosticSuppression

    init(astgen: ASTGenVisitor, suppression: BridgedDiagnosticSuppression, viewMode: SyntaxTreeViewMode) {
      self.astgen = astgen
      self.suppression = suppression

      super.init(viewMode: viewMode)
    }

    override func visitAny(_ node: Syntax) -> SyntaxVisitorContinueKind {
      guard let type = node.as(TypeSyntax.self) else {
        return .visitChildren
      }

      // We are generating just for validation; suppress diagnostics.
      suppression.start()
      let astgenResult = self.astgen.generate(type: type)
      suppression.stop()

      self.astgen.validateGeneratedTypeRepr(astgenResult, from: type)

      return .skipChildren
    }
  }

  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)

  TypeVisitor(
    astgen: ASTGenVisitor(
      diagnosticEngine: diagnosticEngine,
      sourceBuffer: sourceFile.pointee.buffer,
      declContext: declContext,
      astContext: astContext,
      legacyParser: legacyParser
    ),
    suppression: BridgedDiagnosticSuppression.init(diagnosticEngine: diagnosticEngine),
    viewMode: .sourceAccurate
  )
  .walk(sourceFile.pointee.syntax)
}
