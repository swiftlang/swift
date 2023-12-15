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

  func generateWithLegacy(_ node: TypeSyntax) -> BridgedTypeRepr {
    legacyParse.parseType(self.generateSourceLoc(node), self.declContext)
  }

  func generateMatchingPatternWithLegacy(_ node: some PatternSyntaxProtocol) {
    //    legacyParse.parseMatchingPattern(self.bridgedSourceLoc(syntax: node), self.declContext)
  }

  func generateBindingPatternWithLegacy(_ node: some PatternSyntaxProtocol) {
    //    legacyParse.parseBindingPattern(self.bridgedSourceLoc(syntax: node), self.declContext)
  }
}
