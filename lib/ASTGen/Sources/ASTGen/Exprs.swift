import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: ClosureExprSyntax) -> ASTNode {
    let statements = node.statements.map(self.visit).map { $0.bridged() }
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    let body = statements.withBridgedArrayRef { ref in
      BraceStmt_create(ctx, loc, ref, loc)
    }

    return .expr(ClosureExpr_create(ctx, body, declContext))
  }

  public func visit(_ node: FunctionCallExprSyntax) -> ASTNode {
    // Transform the trailing closure into an argument.
    if let trailingClosure = node.trailingClosure {
      let tupleElement = TupleExprElementSyntax(
        label: nil, colon: nil, expression: ExprSyntax(trailingClosure), trailingComma: nil)

      return visit(node.addArgument(tupleElement).withTrailingClosure(nil))
    }

    let args = visit(node.argumentList).rawValue
    // TODO: hack
    let callee = visit(node.calledExpression).rawValue

    return .expr(SwiftFunctionCallExpr_create(self.ctx, callee, args))
  }

  public func visit(_ node: IdentifierExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return .expr(SwiftIdentifierExpr_create(ctx, id, loc))
  }

  public func visit(_ node: IdentifierPatternSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return .expr(SwiftIdentifierExpr_create(ctx, id, loc))
  }

  public func visit(_ node: MemberAccessExprSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let base = visit(node.base!).rawValue
    var nameText = node.name.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return .expr(UnresolvedDotExpr_create(ctx, base, loc, name, loc))
  }

  public func visit(_ node: TupleExprElementListSyntax) -> ASTNode {
    let elements = node.map(self.visit).map { $0.rawValue }

    // TODO: find correct paren locs.
    let lParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    let rParenLoc = self.base.advanced(by: node.position.utf8Offset).raw

    return .expr(
      elements.withBridgedArrayRef { elementsRef in
        SwiftTupleExpr_create(self.ctx, lParenLoc, elementsRef, rParenLoc)
      })
  }
}
