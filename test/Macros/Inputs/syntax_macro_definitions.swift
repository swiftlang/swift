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

extension SimpleDiagnosticMessage: FixItMessage {
  var fixItID: MessageID { diagnosticID }
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

    // Link the folded argument back into the tree.
    var node = node.withArgumentList(node.argumentList.replacing(childAt: 0, with: node.argumentList.first!.withExpression(foldedArgument.as(ExprSyntax.self)!)))

   class AddVisitor: SyntaxRewriter {
      var diagnostics: [Diagnostic] = []

      override func visit(
        _ node: InfixOperatorExprSyntax
      ) -> ExprSyntax {
        if let binOp = node.operatorOperand.as(BinaryOperatorExprSyntax.self) {
          if binOp.operatorToken.text == "+" {
            let messageID = MessageID(domain: "silly", id: "addblock")
            diagnostics.append(
              Diagnostic(
                node: Syntax(node.operatorOperand),
                message: SimpleDiagnosticMessage(
                  message: "blocked an add; did you mean to subtract?",
                  diagnosticID: messageID,
                  severity: .error
                ),
                highlights: [
                  Syntax(node.leftOperand.withoutTrivia()),
                  Syntax(node.rightOperand.withoutTrivia())
                ],
                fixIts: [
                  FixIt(
                    message: SimpleDiagnosticMessage(
                      message: "use '-'",
                      diagnosticID: messageID,
                      severity: .error
                    ),
                    changes: [
                      FixIt.Change.replace(
                        oldNode: Syntax(binOp.operatorToken.withoutTrivia()),
                        newNode: Syntax(
                          TokenSyntax(
                            .spacedBinaryOperator("-"),
                            presence: .present
                          )
                        )
                      )
                    ]
                  ),
                ]
              )
            )

            return ExprSyntax(
              node.withOperatorOperand(
                ExprSyntax(
                  binOp.withOperatorToken(
                    binOp.operatorToken.withKind(.spacedBinaryOperator("-"))
                  )
                )
              )
            )
          }
        }

        return ExprSyntax(node)
      }
    }

    let visitor = AddVisitor()
    let result = visitor.visit(Syntax(node))

    for diag in visitor.diagnostics {
      context.diagnose(diag)
    }

    return result.as(MacroExpansionExprSyntax.self)!.argumentList.first!.expression
  }
}
