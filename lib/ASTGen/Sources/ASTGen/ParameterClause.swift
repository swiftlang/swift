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

fileprivate protocol ValueParameterSyntax: SyntaxProtocol {
  /// The `firstName` with optional type.
  ///
  /// This is the lowest denominator between `FunctionParameterSyntax` and `EnumCaseParameterSyntax`.
  // FIXME: Rename once we support covariant witnesses.
  var optionalFirstName: TokenSyntax? { get }

  var secondName: TokenSyntax? { get }

  /// The `firstName` with optional type.
  ///
  /// This is the lowest denominator between `FunctionParameterSyntax` and `EnumCaseParameterSyntax`.
  // FIXME: Rename once we support covariant witnesses.
  var optionalType: TypeSyntax? { get }

  var defaultValue: InitializerClauseSyntax? { get }
}

extension FunctionParameterSyntax: ValueParameterSyntax {
  fileprivate var optionalFirstName: TokenSyntax? {
    firstName
  }

  fileprivate var optionalType: TypeSyntax? {
    type
  }
}

extension EnumCaseParameterSyntax: ValueParameterSyntax {
  fileprivate var optionalFirstName: TokenSyntax? {
    firstName
  }

  fileprivate var optionalType: TypeSyntax? {
    type
  }
}

extension ASTGenVisitor {
  func visit(_ node: FunctionParameterSyntax) -> ASTNode {
    self.makeParamDecl(node)
  }

  func visit(_ node: EnumCaseParameterSyntax) -> ASTNode {
    self.makeParamDecl(node)
  }

  private func makeParamDecl(_ node: some ValueParameterSyntax) -> ASTNode {
    // FIXME: This location should be derived from the type repr.
    let specifierLoc: BridgedSourceLoc = nil

    let firstName: BridgedIdentifier
    if node.optionalFirstName?.tokenKind == .wildcard {
      // Swift AST represents "_" as a null identifier.
      firstName = nil
    } else {
      firstName = node.optionalFirstName.bridgedIdentifier(in: self)
    }

    let (secondName, secondNameLoc) = node.secondName.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      ParamDecl_create(
        self.ctx,
        self.declContext,
        specifierLoc,
        firstName,
        self.bridgedSourceLoc(for: node.optionalFirstName),
        secondName,
        secondNameLoc,
        self.visit(node.optionalType)?.rawValue,
        self.visit(node.defaultValue?.value)?.rawValue
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

  func visit(_ node: EnumCaseParameterClauseSyntax) -> ASTNode {
    .misc(
      ParameterList_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.leftParen),
        node.parameters.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self),
        self.bridgedSourceLoc(for: node.rightParen)
      )
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func visit(_ node: FunctionParameterListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self)
  }
}
