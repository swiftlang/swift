import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

struct MemberInjectingMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf decl: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let member: DeclSyntax =
      """
      private var _macroInjectedMember: String = ""
      """

    return [member]
  }
}
