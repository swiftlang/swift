//===--- Patterns.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  func generate(pattern node: PatternSyntax, typeAnnotation annotationNode: TypeAnnotationSyntax?) -> BridgedPattern {
    let pat = self.generate(pattern: node)
    if let annotationNode {
      return BridgedTypedPattern.createParsed(
        self.ctx,
        pattern: pat,
        type: self.generate(type: annotationNode.type)
      ).asPattern
    } else {
      return pat
    }
  }

  func generate(pattern node: PatternSyntax) -> BridgedPattern {
    switch node.as(PatternSyntaxEnum.self) {
    case .expressionPattern(let node):
      return self.generate(expressionPattern: node).asPattern
    case .identifierPattern(let node):
      return self.generate(identifierPattern: node).asPattern
    case .isTypePattern(let node):
      return self.generate(isTypePattern: node).asPattern
    case .missingPattern(let node):
      return self.generate(missingPattern: node)
    case .tuplePattern(let node):
      return self.generate(tuplePattern: node)
    case .valueBindingPattern(let node):
      return self.generate(valueBindingPattern: node).asPattern
    case .wildcardPattern(let node):
      return self.generate(wildcardPattern: node).asPattern
    }
  }

  func generate(expressionPattern node: ExpressionPatternSyntax) -> BridgedExprPattern {
    return .createParsed(
      self.declContext,
      expr: self.generate(expr: node.expression)
    )
  }

  func generate(identifierPattern node: IdentifierPatternSyntax) -> BridgedNamedPattern {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.identifier)
    return .createParsed(
      ctx,
      declContext: declContext,
      name: name,
      loc: nameLoc
    )
  }

  func generate(isTypePattern node: IsTypePatternSyntax) -> BridgedIsPattern {
    return .createParsed(
      self.ctx,
      isLoc: self.generateSourceLoc(node.isKeyword),
      typeExpr: .createParsed(
        self.ctx,
        type: self.generate(type: node.type)
      )
    )
  }

  func generate(missingPattern node: MissingPatternSyntax) -> BridgedPattern {
    // Recover by creating implicit '_' pattern.
    return BridgedAnyPattern.createImplicit(self.ctx).asPattern
  }

  func generate(tuplePattern node: TuplePatternSyntax) -> BridgedPattern {
    if node.elements.count == 1, let firstElement = node.elements.first, firstElement.label == nil {
      return BridgedParenPattern.createParsed(
        self.ctx,
        lParenLoc: self.generateSourceLoc(node.leftParen),
        subPattern: self.generate(pattern: firstElement.pattern),
        rParenLoc: self.generateSourceLoc(node.rightParen)
      ).asPattern
    }
    return BridgedTuplePattern.createParsed(
      self.ctx,
      lParenLoc: self.generateSourceLoc(node.leftParen),
      elements: node.elements.lazy.map {
        BridgedTuplePatternElt(
          Label: self.generateIdentifier($0.label),
          LabelLoc: self.generateSourceLoc($0.label),
          ThePattern: self.generate(pattern: $0.pattern)
        )
      }.bridgedArray(in: self),
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asPattern
  }

  func generate(valueBindingPattern node: ValueBindingPatternSyntax) -> BridgedBindingPattern {
    return .createParsed(
      self.ctx,
      keywordLoc: self.generateSourceLoc(node.bindingSpecifier),
      isLet: node.bindingSpecifier.keywordKind == .let,
      subPattern: self.generate(pattern: node.pattern)
    )
  }

  func generate(wildcardPattern node: WildcardPatternSyntax) -> BridgedAnyPattern {
    return .createParsed(
      self.ctx,
      loc: self.generateSourceLoc(node.wildcard)
    )
  }
}
