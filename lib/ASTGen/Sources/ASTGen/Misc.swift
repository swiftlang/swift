import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: MemberDeclListItemSyntax) -> ASTNode {
    visit(Syntax(node.decl))
  }

  public func visit(_ node: TupleExprElementSyntax) -> ASTNode {
    visit(node.expression)
  }

  public func visit(_ node: InitializerClauseSyntax) -> ASTNode {
    visit(node.value)
  }

  public func visit(_ node: ConditionElementSyntax) -> ASTNode {
    visit(node.condition)
  }

  public func visit(_ node: CodeBlockItemSyntax) -> ASTNode {
    visit(node.item)
  }

  public func visit(_ node: ArrayElementSyntax) -> ASTNode {
    visit(node.expression)
  }
}
