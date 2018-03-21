// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import SwiftSyntax
import SwiftLang

func getInput(_ file: String) -> URL {
  var result = URL(fileURLWithPath: #file)
  result.deleteLastPathComponent()
  result.appendPathComponent("Inputs")
  result.appendPathComponent(file)
  return result
}


class FuncRenamer: SyntaxRewriter {
  override func visit(_ node: FunctionDeclSyntax) ->DeclSyntax {
    return (super.visit(node) as! FunctionDeclSyntax).withIdentifier(
      SyntaxFactory.makeIdentifier("anotherName"))
  }
}

var PositionTests = TestSuite("AbsolutePositionTests")

PositionTests.test("Visitor") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(getInput("visitor.swift"))
    let source = try String(contentsOf: getInput("visitor.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    expectEqual(parsed.position.byteOffset, 0)
    expectEqual(parsed.eofToken.positionAfterSkippingLeadingTrivia.byteOffset,
                source.count)
    expectEqual(parsed.position.byteOffset, 0)
    expectEqual(parsed.byteSize, source.count)
  })
}

PositionTests.test("Closure") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(getInput("closure.swift"))
    let source = try String(contentsOf: getInput("closure.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    expectEqual(parsed.eofToken.positionAfterSkippingLeadingTrivia.byteOffset,
                source.count)
    expectEqual(parsed.position.byteOffset, 0)
    expectEqual(parsed.byteSize, source.count)
  })
}

PositionTests.test("Rename") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(getInput("visitor.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    let renamed = FuncRenamer().visit(parsed) as! SourceFileSyntax
    let renamedSource = renamed.description
    expectEqual(renamed.eofToken.positionAfterSkippingLeadingTrivia.byteOffset,
                renamedSource.count)
    expectEqual(renamed.byteSize, renamedSource.count)
  })
}

PositionTests.test("CurrentFile") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(URL(fileURLWithPath: #file))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    class Visitor: SyntaxVisitor {
      override func visitPre(_ node: Syntax) {
        _ = node.position
        _ = node.byteSize
        _ = node.positionAfterSkippingLeadingTrivia
      }
      override func visit(_ node: TokenSyntax) {
        expectEqual(node.position.byteOffset + node.leadingTrivia.byteSize,
                    node.positionAfterSkippingLeadingTrivia.byteOffset)
      }
    }
    Visitor().visit(parsed)
  })
}

PositionTests.test("Recursion") {
  expectDoesNotThrow({
    var l = [CodeBlockItemSyntax]()
    let idx = 2000
    for _ in 0...idx {
      l.append(SyntaxFactory.makeCodeBlockItem(
        item: SyntaxFactory.makeReturnStmt(
          returnKeyword: SyntaxFactory.makeToken(.returnKeyword, presence: .present)
            .withTrailingTrivia(.newlines(1)), expression: nil), semicolon: nil))
    }
    let root = SyntaxFactory.makeSourceFile(
      statements: SyntaxFactory.makeCodeBlockItemList(l),
      eofToken: SyntaxFactory.makeToken(.eof, presence: .present))
    _ = root.statements[idx].position
    _ = root.statements[idx].byteSize
    _ = root.statements[idx].positionAfterSkippingLeadingTrivia
  })
}

func createSourceFile(_ count: Int) -> SourceFileSyntax {
  let leading = Trivia(pieces: [
      .newlines(1),
      .backticks(1),
      .docLineComment("/// some comment")
      ])
  let trailing = Trivia.docLineComment("/// This is comment\n")
  let items : [CodeBlockItemSyntax] =
    [CodeBlockItemSyntax](repeating: CodeBlockItemSyntax {
      $0.useItem(ReturnStmtSyntax {
        $0.useReturnKeyword(
          SyntaxFactory.makeReturnKeyword(
            leadingTrivia: leading,
            trailingTrivia: trailing))
      })}, count: count)
  return SyntaxFactory.makeSourceFile(
    statements: SyntaxFactory.makeCodeBlockItemList(items),
    eofToken: SyntaxFactory.makeToken(.eof, presence: .present))
}

PositionTests.test("Trivias") {
  expectDoesNotThrow({
    let idx = 5
    let root = createSourceFile(idx + 1)
    expectEqual(root.leadingTrivia!.count, 3)
    expectEqual(root.trailingTrivia!.count, 0)
    let state = root.statements[idx]
    expectEqual(state.leadingTrivia!.count, 3)
    expectEqual(state.trailingTrivia!.count, 1)
    expectEqual(state.leadingTrivia!.byteSize + state.trailingTrivia!.byteSize
      + state.byteSizeAfterTrimmingTrivia, state.byteSize)
    expectFalse(root.statements.isImplicit)
  })
}

PositionTests.test("Implicit") {
  expectDoesNotThrow({
    let root = createSourceFile(0)
    expectTrue(root.statements.isImplicit)
  })
}

runAllTests()
