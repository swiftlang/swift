import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: IfStmtSyntax) -> UnsafeMutableRawPointer {
    let conditions = node.conditions.map(self.visit)
    assert(conditions.count == 1) // TODO: handle multiple conditions.
    
    let body = visit(node.body)
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    
    if let elseBody = node.elseBody, node.elseKeyword != nil {
      return IfStmt_create(ctx, loc, conditions.first!, body, loc, visit(elseBody))
    }
    
    return IfStmt_create(ctx, loc, conditions.first!, body, nil, nil)
  }
}
