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
  /// Diagnose a 'where' clause written inside the angle brackets, which is
  /// obsolete, with fix-its that move it to follow the declaration.
  func diagnoseWhereInGenericParameterClause(_ node: GenericParameterClauseSyntax) {
    guard let whereClause = node.genericWhereClause,
      let lastParam = node.parameters.last,
      node.rightAngle.presence == .present
    else {
      return
    }

    // Everything between the last parameter and the closing bracket moves.
    let lastToken = lastParam.trailingComma?.previousToken(viewMode: .sourceAccurate) ?? lastParam.lastToken(viewMode: .sourceAccurate)
    guard let lastToken else { return }
    let removeStart = lastToken.endPositionBeforeTrailingTrivia
    let removeEnd = node.rightAngle.positionAfterSkippingLeadingTrivia
    let removeRange = self.generateCharSourceRange(
      start: removeStart,
      length: SourceLength(utf8Length: removeEnd.utf8Offset - removeStart.utf8Offset)
    )
    let movedText = String(
      decoding: self.base[removeStart.utf8Offset..<removeEnd.utf8Offset],
      as: UTF8.self
    )

    // The declaration's own 'where' clause, if any, absorbs the requirements.
    let trailingWhere: GenericWhereClauseSyntax?
    let insertAfter: TokenSyntax?
    switch node.parent?.as(SyntaxEnum.self) {
    case .functionDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = decl.signature.lastToken(viewMode: .sourceAccurate)
    case .initializerDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = decl.signature.lastToken(viewMode: .sourceAccurate)
    case .subscriptDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = decl.returnClause.lastToken(viewMode: .sourceAccurate)
    case .structDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = (decl.inheritanceClause?.lastToken(viewMode: .sourceAccurate)) ?? node.lastToken(viewMode: .sourceAccurate)
    case .classDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = (decl.inheritanceClause?.lastToken(viewMode: .sourceAccurate)) ?? node.lastToken(viewMode: .sourceAccurate)
    case .enumDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = (decl.inheritanceClause?.lastToken(viewMode: .sourceAccurate)) ?? node.lastToken(viewMode: .sourceAccurate)
    case .actorDecl(let decl):
      trailingWhere = decl.genericWhereClause
      insertAfter = (decl.inheritanceClause?.lastToken(viewMode: .sourceAccurate)) ?? node.lastToken(viewMode: .sourceAccurate)
    default:
      // Other declarations, typealiases included, do not get this diagnostic.
      return
    }

    var fixIts = [CompilerDiagnosticFixIt(remove: removeRange)]
    if let trailingWhere {
      fixIts.append(
        .init(
          replace: self.generateCharSourceRange(trailingWhere.whereKeyword),
          with: whereClause.trimmedDescription + ","
        )
      )
    } else if let insertAfter {
      fixIts.append(
        .init(
          replace: self.generateCharSourceRange(
            start: insertAfter.endPositionBeforeTrailingTrivia,
            length: SourceLength(utf8Length: 0)
          ),
          with: movedText
        )
      )
    }
    self.diagnose(.where_inside_brackets, at: whereClause.whereKeyword, fixIts: fixIts)
  }

  func generate(genericParameterClause node: GenericParameterClauseSyntax) -> BridgedGenericParamList {
    self.diagnoseWhereInGenericParameterClause(node)
    return .createParsed(
      self.ctx,
      leftAngleLoc: self.generateSourceLoc(node.leftAngle),
      parameters: node.parameters.lazy.map(self.generate).bridgedArray(in: self),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      rightAngleLoc: self.generateSourceLoc(node.rightAngle)
    )
  }

  func generate(genericParameter node: GenericParameterSyntax) -> BridgedGenericTypeParamDecl {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)
    self.diagnoseDollarDeclName(node.name)

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

    let decl = BridgedGenericTypeParamDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: self.generateSourceLoc(node.specifier),
      name: name,
      nameLoc: nameLoc,
      inheritedType: self.generate(type: node.inheritedType),
      index: genericParameterIndex,
      paramKind: paramKind
    )

    // Attributes are parsed here and diagnosed by the type checker if they do
    // not apply.
    var attrs = BridgedDeclAttributes()
    self.generateDeclAttributes(attributeList: node.attributes) { attrs.add($0) }
    decl.asDecl.attachParsedAttrs(attrs)
    return decl
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
      // A trivial constraint without any size is not one of exact size 0.
      return .getLayoutConstraint(self.ctx, kind: .trivial)
    }

    let size: Int
    if let parsed = Int(sizeToken.text, radix: 10) {
      size = parsed
    } else {
      // A missing or non-numeric size token is parser recovery output, e.g.
      // `_Trivial(, 8)`. Diagnose and carry on with 0 so the constraint is still
      // well-formed.
      self.diagnose(.layout_size_should_be_positive, at: sizeToken)
      size = 0
    }

    let alignment: Int?
    if let alignmentToken = node.alignment {
      if let parsed = Int(alignmentToken.text, radix: 10) {
        alignment = parsed
      } else {
        // E.g. `_Trivial(64, )`: the alignment is missing or not a number.
        self.diagnose(.layout_alignment_should_be_positive, at: alignmentToken)
        alignment = nil
      }
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
    // A trailing comma leaves the C++ parser expecting another requirement.
    if !self.ctx.langOpts.hasFeature(.TrailingComma), let comma = node.requirements.last?.trailingComma {
      self.diagnose(.expected_type, at: comma.nextToken(viewMode: .sourceAccurate).map(Syntax.init) ?? Syntax(comma))
    }
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
        // Layout constraints are only for '_specialize'.
        if !sequence(first: Syntax(layout).parent, next: { $0?.parent }).contains(where: {
          $0?.as(AttributeSyntax.self)?.attributeName.trimmedDescription == "_specialize"
        }) {
          self.diagnose(.layout_constraints_only_inside_specialize_attr, at: layout.layoutSpecifier)
        }
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
      return self.generateValueOrType(expr: expr)
    }
  }

  func generate(sameTypeRightType node: SameTypeRequirementSyntax.RightType) -> BridgedTypeRepr {
    switch node {
    case .type(let type):
      return self.generate(type: type)

    case .expr(let expr):
      return self.generateValueOrType(expr: expr)
    }
  }

  func generate(genericArgument node: GenericArgumentSyntax.Argument) -> BridgedTypeRepr {
    switch node {
    case .type(let type):
      return self.generate(type: type)

    case .expr(let expr):
      return self.generateValueOrType(expr: expr)
    }
  }

  /// Generate a type from an expression that stands in either for a generic
  /// *value* argument, such as the `2` in `Vector<Int, 2>`, or for a type that
  /// the parser happened to hand back in expression position, such as the
  /// `Count` in `@_rawLayout(likeArrayOf: Element, count: Count)`.
  ///
  /// The C++ parser's equivalent is `parseTypeOrValue`.
  func generateValueOrType(expr node: ExprSyntax) -> BridgedTypeRepr {
    // Generic value argument expressions can be integer literals, or
    // arbitrary parenthesized integer expressions (Literal Expressions).

    // Because arbitrary expressions currently require an experimental
    // language feature (LiteralExpressions), and prefix '-' for integer literals
    // is otherwise supported, we handle the '-' prefix operator here as a special
    // case by turning it into a negative IntegerLiteralExpr.
    if case .prefixOperatorExpr(let prefixNode) = node.as(ExprSyntaxEnum.self),
      prefixNode.operator.text == "-",
      let intLitExpr = prefixNode.expression.as(IntegerLiteralExprSyntax.self)
    {
      let bridgedIntExpr = self.generate(integerLiteralExpr: intLitExpr)
      bridgedIntExpr.setNegative(loc: self.generateSourceLoc(prefixNode.operator))
      return BridgedGenericArgumentExprTypeRepr.createParsed(
        self.ctx,
        expr: bridgedIntExpr.asExpr,
        loc: self.generateSourceLoc(node)
      ).asTypeRepr
    }

    if node.is(IntegerLiteralExprSyntax.self) || ctx.langOpts.hasFeature(.LiteralExpressions) {
      return BridgedGenericArgumentExprTypeRepr.createParsed(
        self.ctx,
        expr: self.generate(expr: node),
        loc: self.generateSourceLoc(node)
      ).asTypeRepr
    }

    // Not a value, so it has to be a type. A generic parameter reference reaches
    // here as an expression whenever the enclosing syntax is an argument list
    // rather than a generic argument clause.
    if let typeRepr = self.generateTypeRepr(expr: node) {
      return typeRepr
    }

    self.diagnose(.expected_type, at: node)
    return BridgedErrorTypeRepr.create(self.ctx, range: self.generateSourceRange(node)).asTypeRepr
  }
}
