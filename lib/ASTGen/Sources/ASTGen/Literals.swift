import CASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: StringLiteralExprSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return .expr(
      segment.withBridgedString { bridgedSegment in
        return StringLiteralExpr_create(ctx, bridgedSegment, loc)
      })
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    var segment = node.literal.text
    return .expr(
      segment.withBridgedString { bridgedSegment in
        return IntegerLiteralExpr_create(ctx, bridgedSegment, loc)
      })
  }

  public func visit(_ node: BooleanLiteralExprSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let value = node.literal == .keyword(.true)
    return .expr(BooleanLiteralExpr_create(ctx, value, loc))
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
}
