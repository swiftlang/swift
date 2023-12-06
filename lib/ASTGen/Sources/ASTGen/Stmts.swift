//===--- Stmts.swift ------------------------------------------------------===//
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
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  func generate(stmt node: StmtSyntax) -> BridgedStmt {
    switch node.as(StmtSyntaxEnum.self) {
    case .breakStmt:
      break
    case .continueStmt:
      break
    case .deferStmt:
      break
    case .discardStmt:
      break
    case .doStmt:
      break
    case .expressionStmt(let node):
      return self.generate(expressionStmt: node)
    case .fallThroughStmt:
      break
    case .forStmt:
      break
    case .guardStmt:
      break
    case .labeledStmt:
      break
    case .missingStmt:
      break
    case .repeatStmt:
      break
    case .returnStmt(let node):
      return self.generate(returnStmt: node).asStmt
    case .thenStmt:
      break
    case .throwStmt:
      break
    case .whileStmt:
      break
    case .yieldStmt:
      break
    }
    return self.generateWithLegacy(node)
  }

  func generate(codeBlock node: CodeBlockSyntax) -> BridgedBraceStmt {
    BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: self.generateSourceLoc(node.leftBrace),
      elements: self.generate(codeBlockItemList: node.statements),
      rBraceLoc: self.generateSourceLoc(node.rightBrace)
    )
  }

  func makeIfStmt(_ node: IfExprSyntax) -> BridgedIfStmt {
    // FIXME: handle multiple coniditons.
    // FIXME: handle non-expression conditions.
    let conditions = node.conditions.map(self.generate(conditionElement:))
    assert(conditions.count == 1)

    return .createParsed(
      self.ctx,
      ifKeywordLoc: self.generateSourceLoc(node.ifKeyword),
      condition: conditions.first!.castToExpr,
      thenStmt: self.generate(codeBlock: node.body),
      elseLoc: self.generateSourceLoc(node.elseKeyword),
      elseStmt: node.elseBody.map {
        switch $0 {
        case .codeBlock(let node):
          return self.generate(codeBlock: node).asStmt
        case .ifExpr(let node):
          return self.makeIfStmt(node).asStmt
        }
      }.asNullable
    )
  }

  func generate(expressionStmt node: ExpressionStmtSyntax) -> BridgedStmt {
    switch Syntax(node.expression).as(SyntaxEnum.self) {
    case .ifExpr(let e):
      return makeIfStmt(e).asStmt
    default:
      fatalError("Unhandled case!")
    }
  }

  func generate(returnStmt node: ReturnStmtSyntax) -> BridgedReturnStmt {
    .createParsed(
      self.ctx,
      returnKeywordLoc: self.generateSourceLoc(node.returnKeyword),
      expr: self.generate(expr: node.expression)
    )
  }
}
