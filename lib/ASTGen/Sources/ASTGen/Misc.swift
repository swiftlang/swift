import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: MemberDeclListItemSyntax) -> UnsafeMutableRawPointer {
    visit(Syntax(node.decl))
  }
  
  public func visit(_ node: TupleExprElementSyntax) -> UnsafeMutableRawPointer {
    visit(node.expression)
  }
  
  public func visit(_ node: InitializerClauseSyntax) -> UnsafeMutableRawPointer {
    visit(node.value)
  }
  
  public func visit(_ node: ConditionElementSyntax) -> UnsafeMutableRawPointer {
    visit(node.condition)
  }

  public func visit(_ node: CodeBlockItemSyntax) -> UnsafeMutableRawPointer {
    visit(node.item)
  }
}
