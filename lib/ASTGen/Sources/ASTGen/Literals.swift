import ASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  public func generate(_ node: StringLiteralExprSyntax) -> BridgedStringLiteralExpr {
    let openDelimiterOrQuoteLoc = (node.openingPounds ?? node.openingQuote).bridgedSourceLoc(in: self)

    // FIXME: Handle interpolated strings.
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return segment.withBridgedString { bridgedSegment in
      return .createParsed(self.ctx, value: bridgedSegment, loc: openDelimiterOrQuoteLoc)
    }
  }

  public func generate(_ node: IntegerLiteralExprSyntax) -> BridgedIntegerLiteralExpr {
    var segment = node.literal.text
    return segment.withBridgedString { bridgedSegment in
      return .createParsed(ctx, value: bridgedSegment, loc: node.literal.bridgedSourceLoc(in: self))
    }
  }

  public func generate(_ node: BooleanLiteralExprSyntax) -> BridgedBooleanLiteralExpr {
    let value = node.literal.tokenKind == .keyword(.true)
    return .createParsed(ctx, value: value, loc: node.literal.bridgedSourceLoc(in: self))
  }

  public func generate(_ node: ArrayExprSyntax) -> BridgedArrayExpr {
    let expressions = node.elements.lazy.map(self.generate)

    let commaLocations = node.elements.compactMap(in: self) {
      $0.trailingComma.bridgedSourceLoc(in: self)
    }

    return .createParsed(
      self.ctx,
      lSquareLoc: node.leftSquare.bridgedSourceLoc(in: self),
      elements: expressions.bridgedArray(in: self),
      commaLocs: commaLocations,
      rSquareLoc: node.rightSquare.bridgedSourceLoc(in: self)
    )
  }

  func generate(_ node: NilLiteralExprSyntax) -> BridgedNilLiteralExpr {
    .createParsed(self.ctx, nilKeywordLoc: node.nilKeyword.bridgedSourceLoc(in: self))
  }
}
