import CASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: StringLiteralExprSyntax) -> ASTNode {
    let openDelimiterOrQuoteLoc = self.bridgedSourceLoc(for: node.openingPounds ?? node.openingQuote)

    // FIXME: Handle interpolated strings.
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return .expr(
      segment.withBridgedString { bridgedSegment in
        return StringLiteralExpr_create(self.ctx, bridgedSegment, openDelimiterOrQuoteLoc)
      }
    )
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> ASTNode {
    var segment = node.literal.text
    return .expr(
      segment.withBridgedString { bridgedSegment in
        return IntegerLiteralExpr_create(ctx, bridgedSegment, self.bridgedSourceLoc(for: node.literal))
      }
    )
  }

  public func visit(_ node: BooleanLiteralExprSyntax) -> ASTNode {
    let value = node.literal.tokenKind == .keyword(.true)
    return .expr(BooleanLiteralExpr_create(ctx, value, bridgedSourceLoc(for: node.literal)))
  }

  public func visit(_ node: ArrayExprSyntax) -> ASTNode {
    let expressions = node.elements.lazy.map {
      self.visit($0).rawValue
    }

    let commaLocations = node.elements.compactMap(in: self) {
      self.bridgedSourceLoc(for: $0.trailingComma)
    }

    return .expr(
      ArrayExpr_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.leftSquare),
        expressions.bridgedArray(in: self),
        commaLocations,
        self.bridgedSourceLoc(for: node.rightSquare)
      )
    )
  }

  func visit(_ node: NilLiteralExprSyntax) -> ASTNode {
    .expr(NilLiteralExpr_create(astContext: self.ctx, nilKeywordLoc: self.bridgedSourceLoc(for: node.nilKeyword)))
  }
}
