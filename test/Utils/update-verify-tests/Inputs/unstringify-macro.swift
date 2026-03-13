import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct UnstringifyPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    do {
      let argumentList = node.arguments!.as(LabeledExprListSyntax.self)!
      let arguments = [LabeledExprSyntax](argumentList)
      let arg = arguments.first!.expression.as(StringLiteralExprSyntax.self)!
      let content = arg.representedLiteralValue!
      return [DeclSyntax("\(raw: content)")]
    }
  }
}
