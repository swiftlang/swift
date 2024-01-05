//===--- Literals.swift ---------------------------------------------------===//
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
import SwiftSyntax

extension ASTGenVisitor {
  func generate(stringLiteralExpr node: StringLiteralExprSyntax) -> BridgedStringLiteralExpr {
    let openDelimiterOrQuoteLoc = self.generateSourceLoc(node.openingPounds ?? node.openingQuote)

    // FIXME: Handle interpolated strings.
    // FIXME: Avoid 'String' instantiation
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return segment.withBridgedString { bridgedSegment in
      return .createParsed(self.ctx, value: bridgedSegment, loc: openDelimiterOrQuoteLoc)
    }
  }

  func generate(integerLiteralExpr node: IntegerLiteralExprSyntax) -> BridgedIntegerLiteralExpr {
    // FIXME: Avoid 'String' instantiation
    // FIXME: Strip '_'.
    var segment = node.literal.text
    return segment.withBridgedString { bridgedSegment in
      return .createParsed(
        ctx,
        value: bridgedSegment,
        loc: self.generateSourceLoc(node.literal)
      )
    }
  }

  func generate(booleanLiteralExpr node: BooleanLiteralExprSyntax) -> BridgedBooleanLiteralExpr {
    let value = node.literal.keywordKind == .true
    return .createParsed(
      ctx,
      value: value,
      loc: self.generateSourceLoc(node.literal)
    )
  }

  func generate(arrayExpr node: ArrayExprSyntax) -> BridgedArrayExpr {
    let expressions = node.elements.lazy.map({ self.generate(expr: $0.expression) })

    let commaLocations = node.elements.compactMap(in: self) {
      self.generateSourceLoc($0.trailingComma)
    }

    return .createParsed(
      self.ctx,
      lSquareLoc: self.generateSourceLoc(node.leftSquare),
      elements: expressions.bridgedArray(in: self),
      commaLocs: commaLocations,
      rSquareLoc: self.generateSourceLoc(node.rightSquare)
    )
  }

  private func generate(dictionaryElement node: DictionaryElementSyntax) -> BridgedTupleExpr {
    return BridgedTupleExpr.createParsedDictionaryElement(
      self.ctx,
      key: self.generate(expr: node.key),
      value: self.generate(expr: node.value)
    )
  }

  public func generate(dictionaryExpr node: DictionaryExprSyntax) -> BridgedDictionaryExpr {
    let lBracketLoc = self.generateSourceLoc(node.leftSquare)
    let rBracketLoc = self.generateSourceLoc(node.rightSquare)
    let elements: BridgedArrayRef
    let colonLocs: BridgedArrayRef

    switch node.content {
    case .colon(_):
      elements = BridgedArrayRef()
      colonLocs = BridgedArrayRef()
    case .elements(let elementNodes):
      elements = elementNodes.lazy
        .map({ self.generate(dictionaryElement: $0).asExpr })
        .bridgedArray(in: self)
      colonLocs = elementNodes.lazy
        .map({ self.generateSourceLoc($0.colon) })
        .bridgedArray(in: self)
    }
    return .createParsed(
      self.ctx,
      lBracketLoc: lBracketLoc,
      elements: elements,
      colonLocs: colonLocs,
      rBracketLoc: rBracketLoc
    )
  }

  func generate(nilLiteralExpr node: NilLiteralExprSyntax) -> BridgedNilLiteralExpr {
    .createParsed(
      self.ctx,
      nilKeywordLoc: self.generateSourceLoc(node.nilKeyword)
    )
  }
}
