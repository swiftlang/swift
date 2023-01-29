import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct PrintMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
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

