import SwiftSyntax
import SwiftSyntaxBuilder
import _SwiftSyntaxMacros

public struct StringifyMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.argumentList.first?.expression else {
      // FIXME: Create a diagnostic for the missing argument?
      return ExprSyntax(macro)
    }

    return "(\(argument), \(StringLiteralExprSyntax(content: argument.description)))"
  }
}

public struct MyWrapperMacro: AccessorDeclarationMacro {
    public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    return []
  }
}

public struct WrapAllProperties: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo parent: DeclSyntax,
    annotating member: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    return []
  }
}

