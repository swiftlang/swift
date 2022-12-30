import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: StringLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return .expr(
      segment.withUTF8 { buf in
        return SwiftStringLiteralExpr_create(ctx, buf.baseAddress, buf.count, loc)
      })
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.digits.text
    return .expr(
      segment.withUTF8 { buf in
        return SwiftIntegerLiteralExpr_create(ctx, buf.baseAddress, buf.count, loc)
      })
  }

  public func visit(_ node: BooleanLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let value = node.booleanLiteral == .trueKeyword()
    return .expr(SwiftBooleanLiteralExpr_create(ctx, value, loc))
  }

  public func visit(_ node: ArrayExprSyntax) -> ASTNode {
    let lLoc = self.base.advanced(by: node.leftSquare.position.utf8Offset).raw
    let rLoc = self.base.advanced(by: node.rightSquare.position.utf8Offset).raw

    let elements = node.elements.map { self.visit($0).rawValue }
    let commas = node.elements
      .compactMap { $0.trailingComma }
      .map {
        self.base.advanced(by: $0.position.utf8Offset).raw
      }

    return elements.withBridgedArrayRef { elementsRef in
      commas.withBridgedArrayRef { commasRef in
        .expr(ArrayExpr_create(ctx, lLoc, elementsRef, commasRef, rLoc))
      }
    }
  }
}
