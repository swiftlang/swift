//===----------------------------------------------------------------------===//
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

import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

// MARK: - ParamDecl

extension ASTGenVisitor {
  func visit(_ node: FunctionParameterSyntax) -> ASTNode {
    // FIXME: This location should be derived from the type repr.
    let specifierLoc: BridgedSourceLoc = nil

    let firstName: BridgedIdentifier
    if node.firstName.tokenKind != .wildcard {
      // Swift AST represents "_" as nil.
      firstName = node.firstName.bridgedIdentifier(in: self)
    } else {
      firstName = nil
    }
    let (secondName, secondNameLoc) = node.secondName.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      ParamDecl_create(
        self.ctx,
        self.declContext,
        specifierLoc,
        firstName,
        self.bridgedSourceLoc(for: node.firstName),
        secondName,
        secondNameLoc,
        self.visit(node.type).rawValue
      )
    )
  }
}

// MARK: - ParameterList

extension ASTGenVisitor {
  func visit(_ node: FunctionParameterClauseSyntax) -> ASTNode {
    .misc(
      ParameterList_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.leftParen),
        self.visit(node.parameters),
        self.bridgedSourceLoc(for: node.rightParen)
      )
    )
  }

  @inline(__always)
  func visit(_ node: FunctionParameterListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self)
  }
}
