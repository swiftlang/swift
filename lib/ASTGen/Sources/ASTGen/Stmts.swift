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
import SwiftSyntax

extension ASTGenVisitor {
  public func generate(_ node: CodeBlockSyntax) -> BridgedBraceStmt {
    BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: node.leftBrace.bridgedSourceLoc(in: self),
      elements: self.generate(node.statements),
      rBraceLoc: node.rightBrace.bridgedSourceLoc(in: self)
    )
  }

  func makeIfStmt(_ node: IfExprSyntax) -> BridgedIfStmt {
    let conditions = node.conditions.map(self.generate)
    assert(conditions.count == 1)  // TODO: handle multiple conditions.

    return .createParsed(
      self.ctx,
      ifKeywordLoc: node.ifKeyword.bridgedSourceLoc(in: self),
      condition: conditions.first!.castToExpr,
      thenStmt: self.generate(node.body).asStmt,
      elseLoc: node.elseKeyword.bridgedSourceLoc(in: self),
      elseStmt: (self.generate(node.elseBody)?.castToStmt).asNullable
    )
  }

  public func generate(_ node: ExpressionStmtSyntax) -> BridgedStmt {
    switch Syntax(node.expression).as(SyntaxEnum.self) {
    case .ifExpr(let e):
      return makeIfStmt(e).asStmt
    default:
      fatalError("Unhandled case!")
    }
  }

  public func generate(_ node: ReturnStmtSyntax) -> BridgedReturnStmt {
    .createParsed(
      self.ctx,
      returnKeywordLoc: node.returnKeyword.bridgedSourceLoc(in: self),
      expr: self.generate(node.expression).asNullable
    )
  }
}
