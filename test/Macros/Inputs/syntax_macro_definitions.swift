import SwiftSyntax
import SwiftSyntaxBuilder
import _SwiftSyntaxMacros

/// Replace the label of the first element in the tuple with the given
/// new label.
private func replaceFirstLabel(
  of tuple: TupleExprElementListSyntax, with newLabel: String
) -> TupleExprElementListSyntax{
  guard let firstElement = tuple.first else {
    return tuple
  }

  return tuple.replacing(
    childAt: 0, with: firstElement.withLabel(.identifier(newLabel)))
}

public struct ColorLiteralMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    let argList = replaceFirstLabel(
      of: macro.argumentList, with: "_colorLiteralRed"
    )
    let initSyntax: ExprSyntax = ".init(\(argList))"
    if let leadingTrivia = macro.leadingTrivia {
      return initSyntax.withLeadingTrivia(leadingTrivia)
    }
    return initSyntax
  }
}

public struct FileIDMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    let fileLiteral: ExprSyntax = #""\#(context.moduleName)/\#(context.fileName)""#
    if let leadingTrivia = macro.leadingTrivia {
      return fileLiteral.withLeadingTrivia(leadingTrivia)
    }
    return fileLiteral
  }
}

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
