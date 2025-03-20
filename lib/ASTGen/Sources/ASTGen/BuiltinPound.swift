//===--- BuiltinPound.swift -----------------------------------------------===//
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

import ASTBridging
import BasicBridging
@_spi(RawSyntax) import SwiftSyntax

extension ASTGenVisitor {
  /// 3 state result:
  ///  * successfully generated: `.generated(.some(T))`
  ///  * handled but failed: `.generated(.none)`
  ///  * fallback to default behavior: `.ignored`
  enum MaybeGenerated<T> {
    case generated(T?)
    case ignored
  }

  func maybeGenerateBuiltinPound(macroExpansionDecl node: MacroExpansionDeclSyntax) -> MaybeGenerated<BridgedASTNode> {
    let result = maybeGenerateBuiltinPound(freestandingMacroExpansion: node)

    switch result {
    case .generated(_):
      guard node.attributes.isEmpty && node.modifiers.isEmpty else {
        // TODO: Diagnose.
        fatalError("attributes not applied")
      }
      break
    case .ignored:
      break
    }

    return result
  }

  /// Handle built in pound keywords.
  func maybeGenerateBuiltinPound(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> MaybeGenerated<BridgedASTNode> {
    let macroNameText = node.macroName.rawText;

    // '#file', '#line' etc.
    let magicIdentifierKind = BridgedMagicIdentifierLiteralKind(from: macroNameText.bridged)
    if magicIdentifierKind != .none {
      let magicIdentifierExpr = self.generateMagicIdentifierExpr(
        freestandingMacroExpansion: node,
        kind: magicIdentifierKind
      )
      return .generated(.expr(magicIdentifierExpr.asExpr))
    }

    // '#colorLiteral' et al.
    let objectLiteralKind = BridgedObjectLiteralKind(from: macroNameText.bridged)
    if objectLiteralKind != .none {
      let objectLiteralExpr = self.generateObjectLiteralExpr(
        freestandingMacroExpansion: node,
        kind: objectLiteralKind
      )
      return .generated(.expr(objectLiteralExpr.asExpr))
    }

    // Other built-in pound syntax.
    let keyword = BridgedPoundKeyword(from: macroNameText.bridged)
    switch keyword {
    case .selector:
      let selectorExpr = self.generateObjCSelectorExpr(freestandingMacroExpansion: node)
      return .generated(.expr(selectorExpr))

    case .keyPath:
      let keypathExpr = self.generateObjCKeyPathExpr(freestandingMacroExpansion: node)
      return .generated(.expr(keypathExpr))

    case .assert where ctx.langOptsHasFeature(.StaticAssert):
      let assertStmtOpt = self.generatePoundAssertStmt(freestandingMacroExpansion: node)
      return .generated(assertStmtOpt.map({ .stmt($0.asStmt) }))

    case .error, .warning:
      self.handlePoundDiagnostic(freestandingMacroExpansion: node, kind: keyword == .error ? .error : .warning)
      // '#error' and '#warning' don't produce an AST node.
      return .generated(nil)

    case ._hasSymbol, .available, .unavailable:
      // TODO: Diagnose
      fatalError("stmt condition outside control flow statement")
      // return .generated(nil)

    case .none, .assert:
      // Not a builtin pound keyword.
      return .ignored

    case .file, .fileID, .filePath, .function, .line, .column, .dsohandle,
        .colorLiteral, .imageLiteral, .fileLiteral:
      // Should be handled above
      fatalError("(compiler bug) unreachable")

    case .if, .elseif, .else, .endif, .sourceLocation:
      fatalError("(compiler bug) builtin pound keyword as macro expansion expr")
    }
  }

  enum PoundDiagnosticKind {
    case warning, error
  }

  func handlePoundDiagnostic(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: PoundDiagnosticKind) {

    switch node.parent?.kind {
    case .codeBlockItem, .memberBlockItem, nil:
      break
    default:
      // TODO: Diagnose.
      fatalError("#error/#warning must be declaration level")
      // return
    }

    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // TODO: Diagnose.
      fatalError("#error/#warning with generic specialization")
    }

    guard node.arguments.count == 1,
          let arg = node.arguments.first,
          arg.label == nil,
          let literal = arg.expression.as(StringLiteralExprSyntax.self),
          let message = literal.representedLiteralValue
    else {
      // TODO: Diagnose.
      fatalError("expected single simple string literal in #error/#warning")
    }

    // Unconditionally emit the diagnostic. Inactive #if regions are not generated.
    self.diagnose(.poundDiagnostic(literal, message: message, isError: kind == .error))
  }

  func generatePoundAssertStmt(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedPoundAssertStmt? {
    assert(self.ctx.langOptsHasFeature(.StaticAssert))
    var args = node.arguments[...]
    let conditionExpr = self.generateConsumingAttrOption(args: &args, label: nil) { conditionNode in
      self.generate(expr: conditionNode)
    }
    guard let conditionExpr else {
      return nil
    }
    let message: BridgedStringRef?
    if !args.isEmpty {
      message = self.generateConsumingSimpleStringLiteralAttrOption(args: &args)
    } else {
      message = nil
    }

    return .createParsed(
      self.ctx,
      range: self.generateSourceRange(node),
      condition: conditionExpr,
      message: message ?? BridgedStringRef()
    )
  }

  func generateMagicIdentifierExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: BridgedMagicIdentifierLiteralKind) -> BridgedMagicIdentifierLiteralExpr {
    guard node.lastToken(viewMode: .sourceAccurate) == node.macroName else {
      // TODO: Diagnose.
      fatalError("magic identifier token with arguments")
    }

    return BridgedMagicIdentifierLiteralExpr.createParsed(
      self.ctx,
      kind: kind,
      loc: self.generateSourceLoc(node.pound)
    )
  }

  func generateObjectLiteralExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: BridgedObjectLiteralKind) -> BridgedObjectLiteralExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // TODO: Diagnose.
      fatalError("object identifier with generic specialization")
    }

    return BridgedObjectLiteralExpr.createParsed(
      self.ctx,
      poundLoc: self.generateSourceLoc(node.pound),
      kind: kind,
      args: self.generateArgumentList(
        leftParen: node.leftParen,
        labeledExprList: node.arguments,
        rightParen: node.rightParen,
        trailingClosure: nil,
        additionalTrailingClosures: nil
      )
    )
  }

  func generateObjCSelectorExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // TODO: Diagnose.
      fatalError("#selector with generic specialization")
    }

    var args = node.arguments[...]
    guard let arg = args.popFirst() else {
      // TODO: Diagnose
      fatalError("expected an argument for #selector")
      // return ErrorExpr
    }
    let kind: BridgedObjCSelectorKind
    switch arg.label?.rawText {
    case nil: kind = .method
    case "getter": kind = .getter
    case "setter": kind = .setter
    case _?:
      // TODO: Diagnose
      fatalError("unexpected argument label in #selector")
      // return ErrorExpr
    }
    let expr = self.generate(expr: arg.expression)
    guard args.isEmpty else {
      // TODO: Diagnose
      fatalError("unexpected argument in #selector")
      // return ErrorExpr
    }
    return BridgedObjCSelectorExpr.createParsed(
      self.ctx,
      kind: kind,
      keywordLoc: self.generateSourceLoc(node.pound),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      modifierLoc: self.generateSourceLoc(arg.label),
      subExpr: expr,
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asExpr
  }

  func generateObjCKeyPathExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // TODO: Diagnose.
      fatalError("#keyPath with generic specialization")
    }

    var names: [BridgedDeclNameRef] = []
    var nameLocs: [BridgedDeclNameLoc] = []

    func collectNames(expr node: ExprSyntax) -> Bool {
      if let declRefExpr = node.as(DeclReferenceExprSyntax.self) {
        let nameAndLoc = self.generateDeclNameRef(declReferenceExpr: declRefExpr)
        names.append(nameAndLoc.name)
        nameLocs.append(nameAndLoc.loc)
        return false
      }
      if let memberExpr = node.as(MemberAccessExprSyntax.self) {
        guard let base = memberExpr.base else {
          // TODO: Diagnose
          fatalError("unexpected expression in #keyPath")
        }
        if collectNames(expr: base) {
          return true
        }
        let nameAndLoc = self.generateDeclNameRef(declReferenceExpr: memberExpr.declName)
        names.append(nameAndLoc.name)
        nameLocs.append(nameAndLoc.loc)
        return false
      }
      // TODO: Diagnose
      fatalError("unexpected expression in #keyPath")
      // return true
    }

    var args = node.arguments[...]
    guard let arg = args.popFirst() else {
      // TODO: Diagnose
      fatalError("expected an argument for #keyPath")
      // return ErrorExpr
    }
    guard arg.label == nil else {
      // TODO: Diagnose
      fatalError("unexpected argument label  #keyPath")

    }
    if /*hadError=*/collectNames(expr: arg.expression) {
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr;
    }

    guard args.isEmpty else {
      // TODO: Diagnose
      fatalError("unexpected argument in #keyPath")
      // return ErrorExpr
    }

    return BridgedKeyPathExpr.createParsedPoundKeyPath(
      self.ctx,
      poundLoc: self.generateSourceLoc(node.pound),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      names: names.lazy.bridgedArray(in: self),
      nameLocs: nameLocs.lazy.bridgedArray(in: self),
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asExpr
  }
}
