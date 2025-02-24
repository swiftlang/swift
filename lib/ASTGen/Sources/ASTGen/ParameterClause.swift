//===--- ParameterClause.swift --------------------------------------------===//
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
  var optionalAttributes: AttributeListSyntax? { get }
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
  fileprivate var optionalAttributes: AttributeListSyntax? {
    attributes
  }
  fileprivate var optionalFirstName: TokenSyntax? {
    firstName
  }

  fileprivate var optionalType: TypeSyntax? {
    type
  }
}

extension EnumCaseParameterSyntax: ValueParameterSyntax {
  fileprivate var optionalAttributes: AttributeListSyntax? {
    nil
  }

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
  fileprivate var optionalAttributes: AttributeListSyntax? {
    attributes
  }

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
  func generate(functionParameter node: FunctionParameterSyntax, for context: ParameterContext, at index: Int) -> BridgedParamDecl {
    // For non-subscripts, the argument name is defaulted to the parameter name.
    let argNameByDefault: Bool
    switch context {
    case .operator, .subscript:
      argNameByDefault = false
    case .function, .initializer, .macro:
      argNameByDefault = true
    }
    return self.makeParamDecl(node, argNameByDefault: argNameByDefault, at: index)
  }

  func generate(enumCaseParameter node: EnumCaseParameterSyntax, at index: Int) -> BridgedParamDecl {
    self.makeParamDecl(node, argNameByDefault: true, at: index)
  }

  func generate(closureParameter node: ClosureParameterSyntax, at index: Int) -> BridgedParamDecl {
    self.makeParamDecl(node, argNameByDefault: false, at: index)
  }

  /// Generate a ParamDecl. If `argNameByDefault` is true, then the parameter's
  /// argument label is inferred from the first name if no second name is present.
  private func makeParamDecl(_ node: some ValueParameterSyntax, argNameByDefault: Bool, at index: Int) -> BridgedParamDecl {
    var attrs = BridgedDeclAttributes()

    // Attributes.
    if let attributes = node.optionalAttributes {
      self.generateDeclAttributes(attributeList: attributes) { attr in
        attrs.add(attr)
      }
    }

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

    let initContext: BridgedDefaultArgumentInitializer?
    let initExpr: BridgedExpr?
    if let defaultValue = node.defaultValue {
      // Create the initializer using the current DeclContext (not the function).
      // The context will be reset to the function later via 'ParameterList::setDeclContextOfParamDecls()'.
      initContext = BridgedDefaultArgumentInitializer.create(declContext: self.declContext, index: index)
      initExpr = self.withDeclContext(initContext!.asDeclContext) {
        self.generate(expr: defaultValue.value)
      }
    } else {
      initContext = nil
      initExpr = nil
    }

    // The decl context will be reset to the function later.
    let param = BridgedParamDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: specifierLoc,
      argName: argName,
      argNameLoc: argNameLoc,
      paramName: paramName,
      paramNameLoc: paramNameLoc,
      defaultValue: initExpr.asNullable,
      defaultValueInitContext: initContext.asNullable
    )
    if let type {
      param.setTypeRepr(type)
    }
    param.asDecl.attachParsedAttrs(attrs)
    return param
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
      defaultValue: nil,
      defaultValueInitContext: nil
    )
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
    var params: [BridgedParamDecl] = []
    params.reserveCapacity(node.parameters.count)
    for (index, node) in node.parameters.enumerated() {
      let param = self.generate(functionParameter: node, for: context, at: index)
      params.append(param)
    }

    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: params.lazy.bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(enumCaseParameterClause node: EnumCaseParameterClauseSyntax) -> BridgedParameterList {
    var params: [BridgedParamDecl] = []
    params.reserveCapacity(node.parameters.count)
    for (index, node) in node.parameters.enumerated() {
      let param = self.generate(enumCaseParameter: node, at: index)
      params.append(param)
    }

    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: params.lazy.bridgedArray(in: self),
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
      defaultValue: nil,
      defaultValueInitContext: nil
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: CollectionOfOne(param).bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(closureParameterClause node: ClosureParameterClauseSyntax) -> BridgedParameterList {
    var params: [BridgedParamDecl] = []
    params.reserveCapacity(node.parameters.count)
    for (index, node) in node.parameters.enumerated() {
      let param = self.generate(closureParameter: node, at: index)
      params.append(param)
    }

    return .createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      parameters: params.lazy.bridgedArray(in: self),
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
