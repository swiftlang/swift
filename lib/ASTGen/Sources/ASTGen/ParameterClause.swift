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

import ASTBridging
import BasicBridging
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

  var ellipsis: TokenSyntax? { get }

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

  fileprivate var ellipsis: TokenSyntax? {
    nil
  }
}

extension ASTGenVisitor {
  func generate(functionParameter node: FunctionParameterSyntax) -> BridgedParamDecl {
    self.makeParamDecl(node)
  }

  func generate(enumCaseParameter node: EnumCaseParameterSyntax) -> BridgedParamDecl {
    self.makeParamDecl(node)
  }

  private func makeParamDecl(_ node: some ValueParameterSyntax) -> BridgedParamDecl {
    // FIXME: This location should be derived from the type repr.
    let specifierLoc: BridgedSourceLoc = nil

    let (firstName, firstNameLoc) =
        self.generateIdentifierAndSourceLoc(node.optionalFirstName)
    let (secondName, secondNameLoc) =
        self.generateIdentifierAndSourceLoc(node.secondName)

    var type = node.optionalType.map(generate(type:))
    if let ellipsis = node.ellipsis, let base = type {
      type = BridgedVarargTypeRepr.createParsed(
        self.ctx,
        base: base,
        ellipsisLoc: self.generateSourceLoc(ellipsis)
      ).asTypeRepr
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: specifierLoc,
      firstName: firstName,
      firstNameLoc: firstNameLoc,
      secondName: secondName,
      secondNameLoc: secondNameLoc,
      type: type.asNullable,
      defaultValue: self.generate(expr: node.defaultValue?.value)
    )
  }
}

// MARK: - ParameterList

extension ASTGenVisitor {
  func generate(functionParameterClause node: FunctionParameterClauseSyntax) -> BridgedParameterList {
    BridgedParameterList.createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: self.generate(functionParameterList: node.parameters),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(enumCaseParameterClause node: EnumCaseParameterClauseSyntax) -> BridgedParameterList {
    BridgedParameterList.createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: node.parameters.lazy.map(self.generate).bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(functionParameterList node: FunctionParameterListSyntax) -> BridgedArrayRef {
    node.lazy.map(self.generate).bridgedArray(in: self)
  }
}
