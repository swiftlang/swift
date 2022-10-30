import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: StringLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return .expr(segment.withUTF8 { buf in
      return SwiftStringLiteralExpr_create(ctx, buf.baseAddress, buf.count, loc)
    })
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.digits.text
    return .expr(segment.withUTF8 { buf in
      return SwiftIntegerLiteralExpr_create(ctx, buf.baseAddress, buf.count, loc)
    })
  }

  public func visit(_ node: BooleanLiteralExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let value = node.booleanLiteral == .trueKeyword()
    return .expr(SwiftBooleanLiteralExpr_create(ctx, value, loc))
  }
}
