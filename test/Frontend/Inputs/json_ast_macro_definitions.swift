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

struct PeerInjectingMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf decl: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let peer: DeclSyntax =
      """
      struct FixedNamePeer {}
      """

    return [peer]
  }
}

struct FreestandingInjectingMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let peer: DeclSyntax =
      """
      struct FixedNameFreestander {}
      """

    return [peer]
  }
}
