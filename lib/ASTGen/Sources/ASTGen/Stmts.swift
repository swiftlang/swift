import CASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: CodeBlockSyntax) -> ASTNode {
    .stmt(
      BraceStmt_create(
        self.ctx,
        node.leftBrace.bridgedSourceLoc(in: self),
        self.visit(node.statements),
        node.rightBrace.bridgedSourceLoc(in: self)
      )
    )
  }

  func makeIfStmt(_ node: IfExprSyntax) -> ASTNode {
    let conditions = node.conditions.map { self.visit($0).rawValue }
    assert(conditions.count == 1)  // TODO: handle multiple conditions.

    return .stmt(
      IfStmt_create(
        self.ctx,
        node.ifKeyword.bridgedSourceLoc(in: self),
        conditions.first!,
        self.visit(node.body).rawValue,
        node.elseKeyword.bridgedSourceLoc(in: self),
        self.visit(node.elseBody)?.rawValue
      )
    )
  }

  public func visit(_ node: ExpressionStmtSyntax) -> ASTNode {
    switch Syntax(node.expression).as(SyntaxEnum.self) {
    case .ifExpr(let e):
      return makeIfStmt(e)
    default:
      fatalError("Unhandled case!")
    }
  }

  public func visit(_ node: ReturnStmtSyntax) -> ASTNode {
    .stmt(
      ReturnStmt_create(
        self.ctx,
        node.returnKeyword.bridgedSourceLoc(in: self),
        self.visit(node.expression)?.rawValue
      )
    )
  }
}
