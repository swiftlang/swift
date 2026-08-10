//===--- ParameterClause.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

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
  /// A parameter context that does not admit a keyword argument, i.e. a parameter
  /// written with both an argument label and a parameter name.
  ///
  /// The raw values are the `%select` indices of
  /// `diag::parameter_operator_keyword_argument`.
  enum KeywordArgumentlessContext: UInt {
    case `operator` = 0
    case closure = 1
    case enumElement = 2
  }

  func generate(functionParameter node: FunctionParameterSyntax, for context: ParameterContext, at index: Int) -> BridgedParamDecl {
    // For non-subscripts, the argument name is defaulted to the parameter name.
    let argNameByDefault: Bool
    switch context {
    case .operator, .subscript:
      argNameByDefault = false
    case .function, .initializer, .macro:
      argNameByDefault = true
    }
    return self.makeParamDecl(
      node,
      argNameByDefault: argNameByDefault,
      keywordArgumentlessContext: context == .operator ? .operator : nil,
      at: index
    )
  }

  func generate(enumCaseParameter node: EnumCaseParameterSyntax, at index: Int) -> BridgedParamDecl {
    self.makeParamDecl(
      node,
      argNameByDefault: true,
      keywordArgumentlessContext: .enumElement,
      at: index
    )
  }

  func generate(closureParameter node: ClosureParameterSyntax, at index: Int) -> BridgedParamDecl {
    self.makeParamDecl(
      node,
      argNameByDefault: false,
      keywordArgumentlessContext: .closure,
      at: index
    )
  }

  /// Generate a ParamDecl. If `argNameByDefault` is true, then the parameter's
  /// argument label is inferred from the first name if no second name is present.
  private func makeParamDecl(
    _ node: some ValueParameterSyntax,
    argNameByDefault: Bool,
    keywordArgumentlessContext: KeywordArgumentlessContext? = nil,
    at index: Int
  ) -> BridgedParamDecl {
    var attrs = BridgedDeclAttributes()

    // Attributes.
    if let attributes = node.optionalAttributes {
      self.generateDeclAttributes(attributeList: attributes) { attr in
        attrs.add(attr)
      }
    }

    // FIXME: This location should be derived from the type repr.
    let specifierLoc: SourceLoc = nil

    let paramName: Identifier
    let paramNameLoc: SourceLoc
    let argName: Identifier
    let argNameLoc: SourceLoc

    // Map the first name and second name to argument name and parameter name.
    // If we have both, use them. If we only have one, then use that as the
    // parameter name, inferring it as the argument name if we're allowed to.
    switch (node.optionalFirstName, node.secondName) {
    // `_ name: T` is not a keyword argument: '_' means the parameter has no
    // argument label, which is allowed everywhere. Only a *named* label is
    // rejected, matching the C++ parser's `!param.FirstName.empty()`.
    case let (argNameNode?, paramNameNode?)
    where keywordArgumentlessContext != nil && self.generateIdentifier(argNameNode) != nil:
      // An operator, closure or enum case cannot have a keyword argument. The
      // C++ parser reports this and then collapses the two names into one,
      // keeping the parameter name; leaving the argument label in place produces
      // a closure parameter with an API name, which Sema asserts against.
      self.diagnose(
        .parameter_operator_keyword_argument(keywordArgumentlessContext!.rawValue),
        at: argNameNode,
        fixIts: [
          .init(
            remove: self.generateCharSourceRange(
              start: argNameNode.positionAfterSkippingLeadingTrivia,
              length: SourceLength(
                utf8Length: paramNameNode.positionAfterSkippingLeadingTrivia.utf8Offset
                  - argNameNode.positionAfterSkippingLeadingTrivia.utf8Offset
              )
            )
          )
        ]
      )
      (paramName, paramNameLoc) = self.generateIdentifierAndSourceLoc(paramNameNode)
      (argName, argNameLoc) = argNameByDefault ? (paramName, paramNameLoc) : (nil, nil)
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

    let parenLocs = self.generateParenLocs(
      leftParen: node.leftParen,
      rightParen: node.rightParen,
      isEmpty: params.isEmpty,
      in: node
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: parenLocs.left,
      parameters: params.lazy.bridgedArray(in: self),
      rightParenLoc: parenLocs.right
    )
  }

  /// The locations of a parameter clause's parentheses, as a pair.
  ///
  /// Three constraints have to hold together. `ParameterList` asserts that both
  /// locations are valid or neither is, so they cannot be decided
  /// independently. A parenthesis the parser synthesized during recovery has a
  /// zero-width position just past the preceding token, which can land on
  /// whatever follows -- the body's '{', say -- making the parameter list's
  /// range overlap the body's and tripping the ASTScope child-ordering check.
  /// And `ParameterList::getSourceRange` falls back to the parameters' own
  /// extent when the left paren is invalid, but yields an *invalid* range if
  /// there are no parameters either, which ASTScope also rejects.
  ///
  /// So: use the real parentheses when both are there. Otherwise report none and
  /// let the range come from the parameters -- unless there are no parameters, in
  /// which case collapse to a zero-width location where the parentheses belong.
  func generateParenLocs(
    leftParen: TokenSyntax?,
    rightParen: TokenSyntax?,
    isEmpty: Bool,
    in clause: some SyntaxProtocol
  ) -> (left: SourceLoc, right: SourceLoc) {
    if let leftParen, leftParen.presence == .present,
      let rightParen, rightParen.presence == .present
    {
      return (self.generateSourceLoc(leftParen), self.generateSourceLoc(rightParen))
    }
    guard isEmpty else {
      return (nil, nil)
    }
    // With no parameters either, the range would be invalid, which ASTScope also
    // rejects, so collapse to a zero-width location. It has to be the *start of a
    // token belonging to the declaration*, not the position where the parentheses
    // would have gone: `getCharSourceRangeOfScope` extends a scope's end with
    // `Lexer::getLocForEndOfToken`, so an end that sits on trailing trivia lexes
    // forward into whatever declaration comes next and overlaps it.
    //
    // The C++ parser ends up at the declaration's name for the same reason, e.g.
    // for `func f` with no parameter clause.
    if let precedingToken = clause.previousToken(viewMode: .sourceAccurate) {
      let loc = self.generateSourceLoc(precedingToken)
      return (loc, loc)
    }
    let loc = self.generateSourceLoc(clause)
    return (loc, loc)
  }

  func generate(enumCaseParameterClause node: EnumCaseParameterClauseSyntax) -> BridgedParameterList {
    var params: [BridgedParamDecl] = []
    params.reserveCapacity(node.parameters.count)
    for (index, node) in node.parameters.enumerated() {
      let param = self.generate(enumCaseParameter: node, at: index)
      params.append(param)
    }

    let parenLocs = self.generateParenLocs(
      leftParen: node.leftParen,
      rightParen: node.rightParen,
      isEmpty: params.isEmpty,
      in: node
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: parenLocs.left,
      parameters: params.lazy.bridgedArray(in: self),
      rightParenLoc: parenLocs.right
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
    let parenLocs = self.generateParenLocs(
      leftParen: node.leftParen,
      rightParen: node.rightParen,
      // An accessor parameter clause always has exactly one parameter.
      isEmpty: false,
      in: node
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: parenLocs.left,
      parameters: CollectionOfOne(param).bridgedArray(in: self),
      rightParenLoc: parenLocs.right
    )
  }

  func generate(closureParameterClause node: ClosureParameterClauseSyntax) -> BridgedParameterList {
    var params: [BridgedParamDecl] = []
    params.reserveCapacity(node.parameters.count)
    for (index, node) in node.parameters.enumerated() {
      let param = self.generate(closureParameter: node, at: index)
      params.append(param)
    }

    let parenLocs = self.generateParenLocs(
      leftParen: node.leftParen,
      rightParen: node.rightParen,
      isEmpty: params.isEmpty,
      in: node
    )
    return .createParsed(
      self.ctx,
      leftParenLoc: parenLocs.left,
      parameters: params.lazy.bridgedArray(in: self),
      rightParenLoc: parenLocs.right
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

// MARK: - YieldList

extension ASTGenVisitor {

  func generate(
    functionYieldClause node: FunctionYieldClauseSyntax?
  ) -> BridgedYieldList? {
    guard let yields = node?.yields else { return nil }

    var yieldTypes: [BridgedTypeRepr] = []
    yieldTypes.reserveCapacity(yields.count)
    for node in yields {
      let yield = self.generate(type: node.type)
      yieldTypes.append(yield)
    }

    return BridgedYieldList.createParsed(
      self.ctx,
      leftParenLoc: self.generateSourceLoc(node?.leftParen),
      yieldTypes: yieldTypes.lazy.bridgedArray(in: self),
      rightParenLoc: self.generateSourceLoc(node?.rightParen)
    )
  }
}
