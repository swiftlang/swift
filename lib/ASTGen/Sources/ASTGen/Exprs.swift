import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: ClosureExprSyntax) -> ASTNode {
    let statements = node.statements.map { self.visit($0).bridged() }
    let body: UnsafeMutableRawPointer = statements.withBridgedArrayRef { ref in
      let startLoc = self.base.advanced(by: node.leftBrace.position.utf8Offset).raw
      let endLoc = self.base.advanced(by: node.rightBrace.position.utf8Offset).raw
      return BraceStmt_create(ctx, startLoc, ref, endLoc)
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
    let elements = node.map { self.visit($0).rawValue }
    let labels: [BridgedIdentifier?] = node.map {
      guard var name = $0.label?.text else {
        return nil
      }
      return name.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
    }
    let labelLocs: [UnsafeMutableRawPointer] = node.map {
      let pos = $0.label?.position ?? $0.position
      return base.advanced(by: pos.utf8Offset).raw
    }

    let lParenLoc = self.base.advanced(by: node.position.utf8Offset).raw
    let rParenLoc = self.base.advanced(by: node.endPosition.utf8Offset).raw

    return .expr(
      elements.withBridgedArrayRef { elementsRef in
        labels.withBridgedArrayRef { labelsRef in
          labelLocs.withBridgedArrayRef { labelLocRef in
            SwiftTupleExpr_create(self.ctx, lParenLoc, elementsRef, labelsRef,
                                  labelLocRef, rParenLoc)
          }
        }
      })
  }
}
