import SwiftDiagnostics
import SwiftOperators
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

struct SimpleDiagnosticMessage: DiagnosticMessage {
  let message: String
  let diagnosticID: MessageID
  let severity: DiagnosticSeverity
}

public struct AddBlocker: ExpressionMacro {
  public static func expansion(
    of node: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = node.argumentList.first?.expression else {
      // FIXME: Create a diagnostic for the missing argument?
      return ExprSyntax(node)
    }

    let opTable = OperatorTable.standardOperators
    let foldedArgument = opTable.foldAll(argument) { error in
      context.diagnose(error.asDiagnostic)
    }

   class AddVisitor: SyntaxVisitor {
      var diagnostics: [Diagnostic] = []
      let startPosition: AbsolutePosition

      init(startPosition: AbsolutePosition) {
        self.startPosition = startPosition
        super.init(viewMode: .sourceAccurate)
      }

      override func visit(
        _ node: InfixOperatorExprSyntax
      ) -> SyntaxVisitorContinueKind {
        if let binOp = node.operatorOperand.as(BinaryOperatorExprSyntax.self) {
          if binOp.operatorToken.text == "+" {
            diagnostics.append(
              Diagnostic(
                node: Syntax(node.operatorOperand),
                position: startPosition + SourceLength(utf8Length: binOp.operatorToken.position.utf8Offset),
                message: SimpleDiagnosticMessage(
                  message: "blocked an add",
                  diagnosticID: MessageID(domain: "silly", id: "addblock"),
                  severity: .error
                ),
                highlights: [
                  Syntax(node.leftOperand.withoutTrivia()),
                  Syntax(node.rightOperand.withoutTrivia())
                ]
              )
            )
          }
        }

        return .visitChildren
      }
    }

    let visitor = AddVisitor(startPosition: argument.position)
    visitor.walk(Syntax(foldedArgument))

    for diag in visitor.diagnostics {
      context.diagnose(diag)
    }

    return argument
  }
}
