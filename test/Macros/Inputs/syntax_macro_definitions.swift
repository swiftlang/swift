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
    childAt: 0, with: firstElement.with(\.label, .identifier(newLabel)))
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
      return initSyntax.with(\.leadingTrivia, leadingTrivia)
    }
    return initSyntax
  }
}

public struct FileIDMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    let fileLiteral: ExprSyntax = #""\#(raw: context.moduleName)/\#(raw: context.fileName)""#
    if let leadingTrivia = macro.leadingTrivia {
      return fileLiteral.with(\.leadingTrivia, leadingTrivia)
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

public enum AddBlocker: ExpressionMacro {
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
    let node = node.with(\.argumentList, node.argumentList.replacing(childAt: 0, with: node.argumentList.first!.with(\.expression, foldedArgument.as(ExprSyntax.self)!)))

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
                  Syntax(node.leftOperand.with(\.leadingTrivia, []).with(\.trailingTrivia, [])),
                  Syntax(node.rightOperand.with(\.leadingTrivia, []).with(\.trailingTrivia, []))
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
                        oldNode: Syntax(binOp.operatorToken.with(\.leadingTrivia, []).with(\.trailingTrivia, [])),
                        newNode: Syntax(
                          TokenSyntax(
                            .binaryOperator("-"),
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
              node.with(
                \.operatorOperand,
                ExprSyntax(
                  binOp.with(
                    \.operatorToken,
                    binOp.operatorToken.withKind(.binaryOperator("-"))
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

public class RecursiveMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.argumentList.first?.expression,
          argument.description == "false" else {
      return ExprSyntax(macro)
    }

    return "()"
  }
}

public class NestedDeclInExprMacro: ExpressionMacro {
  public static func expansion(
    of macro: MacroExpansionExprSyntax, in context: inout MacroExpansionContext
  ) -> ExprSyntax {
    return """
    { () -> Void in
      struct Foo { }
      return ()
    }
    """
  }
}

enum CustomError: Error, CustomStringConvertible {
  case message(String)

  var description: String {
    switch self {
    case .message(let text):
      return text
    }
  }
}

public struct DefineBitwidthNumberedStructsMacro: DeclarationMacro {
  public static func expansion(
    of node: MacroExpansionDeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let firstElement = node.argumentList.first,
          let stringLiteral = firstElement.expression.as(StringLiteralExprSyntax.self),
          stringLiteral.segments.count == 1,
          case let .stringSegment(prefix) = stringLiteral.segments.first else {
      throw CustomError.message("#bitwidthNumberedStructs macro requires a string literal")
    }

    return [8, 16, 32, 64].map { bitwidth in
      """

      struct \(raw: prefix)\(raw: String(bitwidth)) { }
      """
    }
  }
}

public struct PropertyWrapperMacro {}

extension PropertyWrapperMacro: AccessorMacro, Macro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    guard let varDecl = declaration.as(VariableDeclSyntax.self),
      let binding = varDecl.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
      binding.accessor == nil
    else {
      return []
    }

    return [
      """

        get {
          _\(identifier).wrappedValue
        }
      """,
      """

        set {
          _\(identifier).wrappedValue = newValue
        }
      """,
    ]
  }
}

public struct WrapAllProperties: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo parent: DeclSyntax,
    annotating member: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    guard member.is(VariableDeclSyntax.self) else {
      return []
    }

    let wrapperTypeName: String
    if parent.is(ClassDeclSyntax.self) {
      wrapperTypeName = "EnclosingSelfWrapper"
    } else {
      wrapperTypeName = "Wrapper"
    }

    let propertyWrapperAttr = AttributeSyntax(
      attributeName: SimpleTypeIdentifierSyntax(
        name: .identifier(wrapperTypeName)
      )
    )

    return [propertyWrapperAttr]
  }
}

public struct TypeWrapperMacro {}

extension TypeWrapperMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo decl: DeclSyntax,
    annotating member: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    guard let varDecl = member.as(VariableDeclSyntax.self),
      let binding = varDecl.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
      binding.accessor == nil
    else {
      return []
    }

    if identifier.text == "_storage" {
      return []
    }

    let customAttr = AttributeSyntax(
      attributeName: SimpleTypeIdentifierSyntax(
        name: .identifier("accessViaStorage")
      )
    )

    return [customAttr]
  }
}

extension TypeWrapperMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo decl: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let storageVariable: DeclSyntax =
      """
      private var _storage = _Storage()
      """

    return [
      storageVariable,
    ]
  }
}

public struct AccessViaStorageMacro: AccessorMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    guard let varDecl = declaration.as(VariableDeclSyntax.self),
      let binding = varDecl.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
      binding.accessor == nil
    else {
      return []
    }

    if identifier.text == "_storage" {
      return []
    }

    return [
      "get { _storage.\(identifier) }",
      "set { _storage.\(identifier) = newValue }",
    ]
  }
}

public struct AddMembers: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo decl: DeclSyntax,
    in context: inout MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let storageStruct: DeclSyntax =
      """
      struct Storage {}
      """

    let storageVariable: DeclSyntax =
      """
      private var storage = Storage()
      """

    let instanceMethod: DeclSyntax =
      """
      func getStorage() -> Storage {
        print("synthesized method")
        return storage
      }
      """

    let staticMethod: DeclSyntax =
      """
      static func method() {}
      """

    let initDecl: DeclSyntax =
      """
      init() {}
      """

    return [
      storageStruct,
      storageVariable,
      instanceMethod,
      staticMethod,
      initDecl,
    ]
  }
}
