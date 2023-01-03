import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import _SwiftSyntaxMacros

public struct PrintMacro: ExpressionMacro {
  public static func expansion(
    of node: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    let printCalls = node.argumentList.map {
      "print(\($0.expression))"
    }.joined(separator: "\n")
    return
      """
      {
      \(raw: printCalls)
      }()
      """
  }
}

