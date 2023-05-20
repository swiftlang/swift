import CASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: CodeBlockSyntax) -> ASTNode {
    .stmt(
      BraceStmt_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.leftBrace),
        self.visit(node.statements),
        self.bridgedSourceLoc(for: node.rightBrace)
      )
    )
  }

  func makeIfStmt(_ node: IfExprSyntax) -> ASTNode {
    let conditions = node.conditions.map { self.visit($0).rawValue }
    assert(conditions.count == 1)  // TODO: handle multiple conditions.

    return .stmt(
      IfStmt_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.ifKeyword),
        conditions.first!,
        self.visit(node.body).rawValue,
        self.bridgedSourceLoc(for: node.elseKeyword),
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
        self.bridgedSourceLoc(for: node.returnKeyword),
        self.visit(node.expression)?.rawValue
      )
    )
  }
}
