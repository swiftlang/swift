import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct AddStructDeclMacro: DeclarationMacro {
    public static func expansion(
      of node: some FreestandingMacroExpansionSyntax,
      in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        return [
            """
            struct MacroAddedStruct : TestEntity {}
            """
        ]
    }
}

public struct AddPeerStructMacro: PeerMacro {
    public static func expansion(
      of node: AttributeSyntax,
      providingPeersOf declaration: some DeclSyntaxProtocol,
      in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        let name = declaration.declName
        return [
            """
            struct _Peer_\(name) : TestEntity {}
            """
        ]
    }
}

public struct AddExtensionMacro: ExtensionMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    return protocols.map {
      ("""
      extension \(type.trimmed): \($0) {
        struct _Extension_\($0): \($0) {}
      }
      """ as DeclSyntax)
      .cast(ExtensionDeclSyntax.self)
    }
  }
}

extension DeclSyntaxProtocol {
    var declName: TokenSyntax {
        if let varDecl = self.as(VariableDeclSyntax.self),
           let first = varDecl.bindings.first,
           let pattern = first.pattern.as(IdentifierPatternSyntax.self) {
            return pattern.identifier.trimmed
        } else if let funcDecl = self.as(FunctionDeclSyntax.self) {
            return funcDecl.name.trimmed
        } else if let structDecl = self.as(StructDeclSyntax.self) {
            return structDecl.name.trimmed
        }
        fatalError("Not implemented")
    }
}
