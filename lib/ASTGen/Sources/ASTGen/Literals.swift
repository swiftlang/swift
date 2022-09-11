import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: StringLiteralExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.segments.first!.as(StringSegmentSyntax.self)!.content.text
    return segment.withUTF8 { buf in
      let id = SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      return SwiftStringLiteralExpr_create(ctx, id, buf.count, loc)
    }
  }

  public func visit(_ node: IntegerLiteralExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var segment = node.digits.text
    return segment.withUTF8 { buf in
      let id = SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      return SwiftIntegerLiteralExpr_create(ctx, id, buf.count, loc)
    }
  }
}
