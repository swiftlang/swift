// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: sdk_overlay

import Foundation
import StdlibUnittest
import SwiftSyntax

func cannedStructDecl() -> StructDeclSyntax {
  let fooID = SyntaxFactory.makeIdentifier("Foo", trailingTrivia: .spaces(1))
  let structKW = SyntaxFactory.makeStructKeyword(trailingTrivia: .spaces(1))
  let rBrace = SyntaxFactory.makeRightBraceToken(leadingTrivia: .newlines(1))
  return StructDeclSyntax {
    $0.useStructKeyword(structKW)
    $0.useIdentifier(fooID)
    $0.useLeftBrace(SyntaxFactory.makeLeftBraceToken())
    $0.useRightBrace(rBrace)
  }
}

var SyntaxFactoryAPI = TestSuite("SyntaxFactoryAPI")

SyntaxFactoryAPI.test("Generated") {

  let structDecl = cannedStructDecl()

  expectEqual("\(structDecl)",
              """
              struct Foo {
              }
              """)

  let forType = SyntaxFactory.makeIdentifier("for", 
                                             leadingTrivia: .backticks(1),
                                             trailingTrivia: [
                                               .backticks(1), .spaces(1)
                                             ])
  let newBrace = SyntaxFactory.makeRightBraceToken(leadingTrivia: .newlines(2))

  let renamed = structDecl.withIdentifier(forType)
                          .withRightBrace(newBrace)

  expectEqual("\(renamed)",
              """
              struct `for` {

              }
              """)

  expectNotEqual(structDecl.leftBrace, renamed.leftBrace)
  expectEqual(structDecl, structDecl.root)
  expectNil(structDecl.parent)
  expectNotNil(structDecl.leftBrace.parent)
  expectEqual(structDecl.leftBrace.parent, structDecl)

  // Ensure that accessing children via named identifiers is exactly the
  // same as accessing them as their underlying data.
  expectEqual(structDecl.leftBrace, structDecl.child(at: 7))
  
  expectEqual("\(structDecl.rightBrace)",
              """

              }
              """)
}

SyntaxFactoryAPI.test("TokenSyntax") {
  let tok = SyntaxFactory.makeStructKeyword()
  expectEqual("\(tok)", "struct")
  expectTrue(tok.isPresent)

  let preSpacedTok = tok.withLeadingTrivia(.spaces(3))
  expectEqual("\(preSpacedTok)", "   struct")

  let postSpacedTok = tok.withTrailingTrivia(.spaces(6))
  expectEqual("\(postSpacedTok)", "struct      ")

  let prePostSpacedTok = preSpacedTok.withTrailingTrivia(.spaces(4))
  expectEqual("\(prePostSpacedTok)", "   struct    ")
}

SyntaxFactoryAPI.test("FunctionCallSyntaxBuilder") {
  let string = SyntaxFactory.makeStringLiteralExpr("Hello, world!")
  let printID = SyntaxFactory.makeVariableExpr("print")
  let arg = FunctionCallArgumentSyntax {
    $0.useExpression(string)
  }
  let call = FunctionCallExprSyntax {
    $0.useCalledExpression(printID)
    $0.useLeftParen(SyntaxFactory.makeLeftParenToken())
    $0.addFunctionCallArgument(arg)
    $0.useRightParen(SyntaxFactory.makeRightParenToken())
  }
  expectEqual("\(call)", "print(\"Hello, world!\")")

  let terminatorArg = FunctionCallArgumentSyntax {
    $0.useLabel(SyntaxFactory.makeIdentifier("terminator"))
    $0.useColon(SyntaxFactory.makeColonToken(trailingTrivia: .spaces(1)))
    $0.useExpression(SyntaxFactory.makeStringLiteralExpr(" "))
  }
  let callWithTerminator = call.withArgumentList(
    SyntaxFactory.makeFunctionCallArgumentList([
      arg.withTrailingComma(
        SyntaxFactory.makeCommaToken(trailingTrivia: .spaces(1))),
      terminatorArg
    ])
  )

  expectEqual("\(callWithTerminator)",
              "print(\"Hello, world!\", terminator: \" \")")
}

runAllTests()
