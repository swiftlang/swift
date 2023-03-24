import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros


public struct CrashingMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    let arg: UInt = UInt(macro.argumentList.first!.expression.description)!
    let zero: UInt = 0
    return "\(raw: zero - arg).description"
  }
}
