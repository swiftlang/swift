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

extension ClosureParameterSyntax: ValueParameterSyntax {
  fileprivate var optionalFirstName: TokenSyntax? {
    self.firstName
  }

  fileprivate var optionalType: TypeSyntax? {
    self.type
  }

  var defaultValue: SwiftSyntax.InitializerClauseSyntax? {
    nil
  }
}

extension ASTGenVisitor {
  func generate(functionParameter node: FunctionParameterSyntax, for context: ParameterContext) -> BridgedParamDecl {
    // For non-subscripts, the argument name is defaulted to the parameter name.
    let argNameByDefault: Bool
    switch context {
    case .operator, .subscript:
      argNameByDefault = false
    case .function, .initializer, .macro:
      argNameByDefault = true
    }
    return self.makeParamDecl(node, argNameByDefault: argNameByDefault)
  }

  func generate(enumCaseParameter node: EnumCaseParameterSyntax) -> BridgedParamDecl {
    self.makeParamDecl(node, argNameByDefault: true)
  }

  func generate(closureParameter node: ClosureParameterSyntax) -> BridgedParamDecl {
    self.makeParamDecl(node, argNameByDefault: false)
  }

  /// Generate a ParamDecl. If `argNameByDefault` is true, then the parameter's
  /// argument label is inferred from the first name if no second name is present.
  private func makeParamDecl(_ node: some ValueParameterSyntax, argNameByDefault: Bool) -> BridgedParamDecl {
    // FIXME: This location should be derived from the type repr.
    let specifierLoc: BridgedSourceLoc = nil

    let paramName: BridgedIdentifier
    let paramNameLoc: BridgedSourceLoc
    let argName: BridgedIdentifier
    let argNameLoc: BridgedSourceLoc

    // Map the first name and second name to argument name and parameter name.
    // If we have both, use them. If we only have one, then use that as the
    // parameter name, inferring it as the argument name if we're allowed to.
    switch (node.optionalFirstName, node.secondName) {
    case let (argNameNode?, paramNameNode?):
      (argName, argNameLoc) = self.generateIdentifierAndSourceLoc(argNameNode)
      (paramName, paramNameLoc) = self.generateIdentifierAndSourceLoc(paramNameNode)
    case (let nameNode?, nil), (nil, let nameNode?):
      // The (nil, nameNode?) case should never happen (since a single label
      // should be the firstName), but treat it the same it to be defensive.
      (paramName, paramNameLoc) = self.generateIdentifierAndSourceLoc(nameNode)
      (argName, argNameLoc) = argNameByDefault ? (paramName, paramNameLoc) : (nil, nil)
    default:
      (argName, argNameLoc, paramName, paramNameLoc) = (nil, nil, nil, nil)
    }

    var type = node.optionalType.map(generate(type:))
    if let ellipsis = node.ellipsis, let base = type {
      type =
        BridgedVarargTypeRepr.createParsed(
          self.ctx,
          base: base,
          ellipsisLoc: self.generateSourceLoc(ellipsis)
        ).asTypeRepr
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: specifierLoc,
      argName: argName,
      argNameLoc: argNameLoc,
      paramName: paramName,
      paramNameLoc: paramNameLoc,
      type: type.asNullable,
      defaultValue: self.generate(expr: node.defaultValue?.value)
    )
  }

  func generate(closureShorthandParameter node : ClosureShorthandParameterSyntax) -> BridgedParamDecl {
    let name = self.generateIdentifierAndSourceLoc(node.name)
    let param = BridgedParamDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: nil,
      argName: nil,
      argNameLoc: nil,
      paramName: name.identifier,
      paramNameLoc: name.sourceLoc,
      type: nil,
      defaultValue: nil
    )
    param.setSpecifier(.default)
    return param
  }
}

// MARK: - ParameterList

extension ASTGenVisitor {

  enum ParameterContext {
    case function
    case initializer
    case macro
    case `subscript`
    case `operator`
  }
  func generate(
    functionParameterClause node: FunctionParameterClauseSyntax,
    for context: ParameterContext
  ) -> BridgedParameterList {
    BridgedParameterList.createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: self.generate(functionParameterList: node.parameters, for: context),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(enumCaseParameterClause node: EnumCaseParameterClauseSyntax) -> BridgedParameterList {
    BridgedParameterList.createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: node.parameters.lazy.map(self.generate(enumCaseParameter:)).bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(accessorParameters node: AccessorParametersSyntax) -> BridgedParameterList {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)
    let param = BridgedParamDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: nil,
      argName: nil,
      argNameLoc: nil,
      paramName: name,
      paramNameLoc: nameLoc,
      type: nil,
      defaultValue: nil
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: CollectionOfOne(param).bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(closureParameterClause node: ClosureParameterClauseSyntax) -> BridgedParameterList {
    let params = node.parameters.lazy.map { node -> BridgedParamDecl in
      let param = self.generate(closureParameter: node)
      param.setSpecifier(.default)
      return param
    }

    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: params.bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(closureShorthandParameterList node: ClosureShorthandParameterListSyntax) -> BridgedParameterList {
    BridgedParameterList.createParsed(
      self.ctx,
      leftParenLoc: nil,
      parameters: node.lazy.map(self.generate(closureShorthandParameter:)).bridgedArray(in: self),
      rightParenLoc: nil
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(functionParameterList node: FunctionParameterListSyntax, for context: ParameterContext) -> BridgedArrayRef {
    node.lazy.map({ self.generate(functionParameter: $0, for: context) }).bridgedArray(in: self)
  }
}
