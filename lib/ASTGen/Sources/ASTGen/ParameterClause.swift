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
  func generate(_ node: FunctionParameterSyntax) -> ASTNode {
    self.makeParamDecl(node)
  }

  func generate(_ node: EnumCaseParameterSyntax) -> ASTNode {
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
        astContext: self.ctx,
        declContext: self.declContext,
        specifierLoc: specifierLoc,
        firstName: firstName,
        firstNameLoc: node.optionalFirstName.bridgedSourceLoc(in: self),
        secondName: secondName,
        secondNameLoc: secondNameLoc,
        type: self.generate(node.optionalType)?.rawValue,
        defaultValue: self.generate(node.defaultValue?.value)?.rawValue
      )
    )
  }
}

// MARK: - ParameterList

extension ASTGenVisitor {
  func generate(_ node: FunctionParameterClauseSyntax) -> ASTNode {
    .misc(
      ParameterList_create(
        astContext: self.ctx,
        leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
        parameters: self.generate(node.parameters),
        rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
      )
    )
  }

  func generate(_ node: EnumCaseParameterClauseSyntax) -> ASTNode {
    .misc(
      ParameterList_create(
        astContext: self.ctx,
        leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
        parameters: node.parameters.lazy.map { self.generate($0).rawValue }.bridgedArray(in: self),
        rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
      )
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(_ node: FunctionParameterListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate($0).rawValue }.bridgedArray(in: self)
  }
}
