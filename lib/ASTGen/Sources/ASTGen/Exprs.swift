import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: ClosureExprSyntax) -> ASTNode {
    let body = BraceStmt_create(
      self.ctx,
      self.bridgedSourceLoc(for: node.leftBrace),
      self.visit(node.statements),
      self.bridgedSourceLoc(for: node.rightBrace)
    )

    // FIXME: Translate the signature, capture list, 'in' location, etc.
    return .expr(ClosureExpr_create(self.ctx, body, self.declContext))
  }

  public func visit(_ node: FunctionCallExprSyntax) -> ASTNode {
    // Transform the trailing closure into an argument.
    if let trailingClosure = node.trailingClosure {
      let tupleElement = LabeledExprSyntax(
        label: nil, colon: nil, expression: ExprSyntax(trailingClosure), trailingComma: nil)

      var node = node
      node.arguments.append(tupleElement)
      node.trailingClosure = nil
      return visit(node)
    }

    let args = visit(node.arguments).rawValue
    let callee = visit(node.calledExpression).rawValue

    return .expr(FunctionCallExpr_create(self.ctx, callee, args))
  }

  public func visit(_ node: DeclReferenceExprSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let id = node.baseName.bridgedIdentifier(in: self)

    return .expr(IdentifierExpr_create(ctx, id, loc))
  }

  public func visit(_ node: IdentifierPatternSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let id = node.identifier.bridgedIdentifier(in: self)

    return .expr(IdentifierExpr_create(ctx, id, loc))
  }

  public func visit(_ node: MemberAccessExprSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let base = visit(node.base!).rawValue
    let name = node.declName.baseName.bridgedIdentifier(in: self)

    return .expr(UnresolvedDotExpr_create(ctx, base, loc, name, loc))
  }

  public func visit(_ node: IfExprSyntax) -> ASTNode {
    let stmt = makeIfStmt(node).rawValue

    // Wrap in a SingleValueStmtExpr to embed as an expression.
    let sve = SingleValueStmtExpr_createWithWrappedBranches(
      ctx, stmt, declContext, /*mustBeExpr*/ true)
    return .expr(sve)
  }

  public func visit(_ node: LabeledExprListSyntax) -> ASTNode {
    let lParenLoc = bridgedSourceLoc(for: node)
    let rParenLoc = bridgedSourceLoc(at: node.endPosition)

    let expressions = node.lazy.map {
      self.visit($0.expression).rawValue
    }
    let labels = node.lazy.map {
      $0.label.bridgedIdentifier(in: self)
    }
    let labelLocations = node.lazy.map {
      if let label = $0.label {
        return self.bridgedSourceLoc(for: label)
      }

      return self.bridgedSourceLoc(for: $0)
    }

    return .expr(
      TupleExpr_create(
        self.ctx,
        lParenLoc,
        expressions.bridgedArray(in: self),
        labels.bridgedArray(in: self),
        labelLocations.bridgedArray(in: self),
        rParenLoc
      )
    )
  }
}
