//===--- Generics.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging

@_spi(ExperimentalLanguageFeatures)
@_spi(RawSyntax)
import SwiftSyntax

extension ASTGenVisitor {
  func generate(genericParameterClause node: GenericParameterClauseSyntax) -> BridgedGenericParamList {
    .createParsed(
      self.ctx,
      leftAngleLoc: self.generateSourceLoc(node.leftAngle),
      parameters: node.parameters.lazy.map(self.generate).bridgedArray(in: self),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      rightAngleLoc: self.generateSourceLoc(node.rightAngle)
    )
  }

  func generate(genericParameter node: GenericParameterSyntax) -> BridgedGenericTypeParamDecl {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    var genericParameterIndex: Int?
    for (index, sibling) in (node.parent?.as(GenericParameterListSyntax.self) ?? []).enumerated() {
      if sibling == node {
        genericParameterIndex = index
        break
      }
    }
    guard let genericParameterIndex = genericParameterIndex else {
      preconditionFailure("Node not part of the parent?")
    }

    let paramKind: swift.GenericTypeParamKind =
      if node.specifier?.tokenKind == .keyword(.each) {
        .pack
      } else if node.specifier?.tokenKind == .keyword(.let) {
        .value
      } else {
        .type
      }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: self.generateSourceLoc(node.specifier),
      name: name,
      nameLoc: nameLoc,
      inheritedType: self.generate(type: node.inheritedType),
      index: genericParameterIndex,
      paramKind: paramKind
    )
  }

  func generate(layoutRequirement node: LayoutRequirementSyntax) -> BridgedLayoutConstraint {
    let id = self.ctx.getIdentifier(node.layoutSpecifier.rawText.bridged)
    let constraint = BridgedLayoutConstraint.getLayoutConstraint(self.ctx, id: id)

    if constraint.isNull || !constraint.isKnownLayout {
      fatalError("(compiler bug) invalid layout requirement")
    }

    if !constraint.isTrivial {
      guard node.size == nil, node.alignment == nil else {
        // TODO: Diagnostics.
        fatalError("(compiler bug) non-trivial layout constraint with arguments")
      }
      return constraint
    }

    guard let sizeToken = node.size else {
      guard node.alignment == nil else {
        // TODO: Diagnostics.
        fatalError("(compiler bug) size is nil, but alignment is not nil?")
      }
      return constraint
    }

    let size: Int
    guard let parsed = Int(sizeToken.text, radix: 10) else {
      fatalError("(compiler bug) invalid size integer literal for a layout constraint")
    }
    size = parsed

    let alignment: Int?
    if let alignmentToken = node.alignment {
      guard let parsed = Int(alignmentToken.text, radix: 10) else {
        fatalError("(compiler bug) invalid alignment integer literal for a layout constraint")
      }
      alignment = parsed
    } else {
      alignment = nil
    }

    return .getLayoutConstraint(
      self.ctx,
      kind: constraint.kind,
      size: size,
      alignment: alignment ?? 0
    )
  }

  func generate(genericWhereClause node: GenericWhereClauseSyntax) -> BridgedTrailingWhereClause {
    let requirements  = node.requirements.lazy.map { elem -> BridgedRequirementRepr in

      // Unwrap 'repeat T' to  (isRequirementExpansion: true, type: T)
      func generateIsExpansionPattern<Node: SyntaxProtocol>(type node: Node) -> (isExpansionPattern: Bool, type: Node) {
        if let expansion = node.as(PackExpansionTypeSyntax.self) {
          // Force unwrapping is safe because both 'TypeSyntax' and 'SameTypeRequirementSyntax.LeftType' accept 'TypeSyntax'.
          return (true, Node(expansion.repetitionPattern)!)
        }
        return (false, node)
      }

      switch elem.requirement {
      case .conformanceRequirement(let conformance):
        let (isExpansionPattern, leftType) = generateIsExpansionPattern(type: conformance.leftType)
        return .createTypeConstraint(
          subject: self.generate(type: leftType),
          colonLoc: self.generateSourceLoc(conformance.colon),
          constraint: self.generate(type: conformance.rightType),
          isExpansionPattern: isExpansionPattern
        )
      case .sameTypeRequirement(let sameType):
        let (isExpansionPattern, leftType) = generateIsExpansionPattern(type: sameType.leftType)
        return .createSameType(
          firstType: self.generate(sameTypeLeftType: leftType),
          equalLoc: self.generateSourceLoc(sameType.equal),
          secondType: self.generate(sameTypeRightType: sameType.rightType),
          isExpansionPattern: isExpansionPattern
        )
      case .layoutRequirement(let layout):
        let (isExpansionPattern, leftType) = generateIsExpansionPattern(type: layout.type)
        return .createLayoutConstraint(
          subject: self.generate(type: leftType),
          colonLoc: self.generateSourceLoc(layout.colon),
          layout: self.generate(layoutRequirement: layout),
          layoutLoc: self.generateSourceLoc(layout.layoutSpecifier),
          isExpansionPattern: isExpansionPattern
        )
      }
    }

    return BridgedTrailingWhereClause.createParsed(
      self.ctx,
      whereKeywordLoc: self.generateSourceLoc(node.whereKeyword),
      requirements: requirements.bridgedArray(in: self)
    )
  }

  func generate(sameTypeLeftType node: SameTypeRequirementSyntax.LeftType) -> BridgedTypeRepr {
    switch node {
    case .type(let type):
      return self.generate(type: type)

    case .expr(let expr):
      return self.generateIntegerType(expr: expr).asTypeRepr
    }
  }

  func generate(sameTypeRightType node: SameTypeRequirementSyntax.RightType) -> BridgedTypeRepr {
    switch node {
    case .type(let type):
      return self.generate(type: type)

    case .expr(let expr):
      return self.generateIntegerType(expr: expr).asTypeRepr
    }
  }

  func generate(genericArgument node: GenericArgumentSyntax.Argument) -> BridgedTypeRepr {
    switch node {
    case .type(let type):
      return self.generate(type: type)

    case .expr(let expr):
      return self.generateIntegerType(expr: expr).asTypeRepr
    }
  }

  func generateIntegerType(expr node: ExprSyntax) -> BridgedIntegerTypeRepr {
    var minusLoc = SourceLoc()
    let literalExpr: IntegerLiteralExprSyntax

    // The only expressions generic argument types support right now are
    // integer literals, '123', and prefix operators for negative integer
    // literals, '-123'.
    switch node.as(ExprSyntaxEnum.self) {
    case .integerLiteralExpr(let node):
      literalExpr = node

    case .prefixOperatorExpr(let node):
      let op = node.operator

      guard op.text == "-" else {
        fatalError("Unknown prefix operator for generic argument type")
      }

      guard let node = node.expression.as(IntegerLiteralExprSyntax.self) else {
        fatalError("Unknown expression kind for generic argument type")
      }

      minusLoc = self.generateSourceLoc(op)
      literalExpr = node

    default:
      fatalError("Unknown expression kind for generic argument type")
    }

    return .createParsed(
      self.ctx,
      string: self.copyAndStripUnderscores(text: literalExpr.literal.rawText),
      loc: self.generateSourceLoc(literalExpr),
      minusLoc: minusLoc
    )
  }
}
