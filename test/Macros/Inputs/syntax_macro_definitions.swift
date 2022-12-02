import SwiftSyntax
import SwiftSyntaxBuilder
import _SwiftSyntaxMacros

public struct FileIDMacro: ExpressionMacro {
  public static func apply(
    _ macro: MacroExpansionExprSyntax, in context: MacroEvaluationContext
  ) -> MacroResult<ExprSyntax> {
    var fileName = context.sourceLocationConverter.location(
      for: .init(utf8Offset: 0)
    ).file ?? "<unknown file>"

    // Only keep everything after the last slash.
    if let lastSlash = fileName.lastIndex(of: "/") {
      fileName = String(fileName[fileName.index(after: lastSlash)...])
    }

    let fileLiteral: ExprSyntax = #""\#(context.moduleName)/\#(fileName)""#
    if let leadingTrivia = macro.leadingTrivia {
      return MacroResult(fileLiteral.withLeadingTrivia(leadingTrivia))
    }
    return MacroResult(fileLiteral)
  }
}
