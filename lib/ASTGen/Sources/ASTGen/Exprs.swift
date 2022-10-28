import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: ClosureExprSyntax) -> UnsafeMutableRawPointer {
    let statements = node.statements.map(self.visit)
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    let body = statements.withBridgedArrayRef { ref in
        BraceStmt_createExpr(ctx, loc, ref, loc)
    }
    
    return ClosureExpr_create(ctx, body, declContext)
  }

  public func visit(_ node: FunctionCallExprSyntax) -> UnsafeMutableRawPointer {
    // Transform the trailing closure into an argument.
    if let trailingClosure = node.trailingClosure {
      let tupleElement = TupleExprElementSyntax(label: nil, colon: nil, expression: ExprSyntax(trailingClosure), trailingComma: nil)
      
      return visit(node.addArgument(tupleElement).withTrailingClosure(nil))
    }
    
    let args = visit(node.argumentList)
    // TODO: hack
    let callee = visit(node.calledExpression)

    return SwiftFunctionCallExpr_create(self.ctx, callee, args)
  }

  public func visit(_ node: IdentifierExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SwiftIdentifierExpr_create(ctx, id, loc)
  }

  public func visit(_ node: IdentifierPatternSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    var text = node.identifier.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SwiftIdentifierExpr_create(ctx, id, loc)
  }
  
  public func visit(_ node: MemberAccessExprSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let base = visit(node.base!)
    var nameText = node.name.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    
    return UnresolvedDotExpr_create(ctx, base, loc, name, loc)
  }

  public func visit(_ node: TupleExprElementListSyntax) -> UnsafeMutableRawPointer {
    let elements = node.map(self.visit)
    
    // TODO: find correct paren locs.
    let lParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    let rParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    
    return elements.withBridgedArrayRef { elementsRef in
      SwiftTupleExpr_create(self.ctx, lParenLoc, elementsRef, rParenLoc)
    }
  }
}
