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
  public func generate(stringLiteralExpr node: StringLiteralExprSyntax) -> BridgedStringLiteralExpr {
    let openDelimiterOrQuoteLoc = self.generateSourceLoc(node.openingPounds ?? node.openingQuote)

    // FIXME: Handle interpolated strings.
    // FIXME: Avoid 'String' instantiation
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return segment.withBridgedString { bridgedSegment in
      return .createParsed(self.ctx, value: bridgedSegment, loc: openDelimiterOrQuoteLoc)
    }
  }

  public func generate(integerLiteralExpr node: IntegerLiteralExprSyntax) -> BridgedIntegerLiteralExpr {
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

  public func generate(booleanLiteralExpr node: BooleanLiteralExprSyntax) -> BridgedBooleanLiteralExpr {
    let value = node.literal.tokenKind == .keyword(.true)
    return .createParsed(
      ctx,
      value: value,
      loc: self.generateSourceLoc(node.literal)
    )
  }

  public func generate(arrayExpr node: ArrayExprSyntax) -> BridgedArrayExpr {
    let expressions = node.elements.lazy.map(self.generate)

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

  func generate(nilLiteralExpr node: NilLiteralExprSyntax) -> BridgedNilLiteralExpr {
    .createParsed(
      self.ctx,
      nilKeywordLoc: self.generateSourceLoc(node.nilKeyword)
    )
  }
}
