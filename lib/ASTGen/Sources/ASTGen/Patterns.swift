
import ASTBridging
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  func generate(pattern node: PatternSyntax) -> ASTNode {
    switch node.as(PatternSyntaxEnum.self) {
    case .expressionPattern(_):
      break
    case .identifierPattern(let node):
      // FIXME: Generate proper pattern instead of temporary Expr.
      return .expr(self.generate(node).asExpr)
    case .isTypePattern(_):
      break
    case .missingPattern(_):
      break
    case .tuplePattern(_):
      break
    case .valueBindingPattern(_):
      break
    case .wildcardPattern(_):
      break
    }
    fatalError("unimplmented")
  }
}


