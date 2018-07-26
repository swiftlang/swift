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

func getSyntaxTree(_ url: URL) throws -> SourceFileSyntax {
  let content = try SwiftLang.parse(path: url.path).data(using: .utf8)!
  return try SyntaxTreeDeserializer().deserialize(content,
                                                  serializationFormat: .json)
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
    let source = try String(contentsOf: getInput("visitor.swift"))
    let parsed = try getSyntaxTree(getInput("visitor.swift"))
    expectEqual(0, parsed.position.utf8Offset)
    expectEqual(source.count,
      parsed.eofToken.positionAfterSkippingLeadingTrivia.utf8Offset)
    expectEqual(0, parsed.position.utf8Offset)
    expectEqual(source.count, parsed.byteSize)
  })
}

PositionTests.test("Closure") {
  expectDoesNotThrow({
    let source = try String(contentsOf: getInput("closure.swift"))
    let parsed = try getSyntaxTree(getInput("closure.swift"))
    expectEqual(source.count, 
      parsed.eofToken.positionAfterSkippingLeadingTrivia.utf8Offset)
    expectEqual(0, parsed.position.utf8Offset)
    expectEqual(source.count, parsed.byteSize)
  })
}

PositionTests.test("Rename") {
  expectDoesNotThrow({
    let parsed = try getSyntaxTree(getInput("visitor.swift"))
    let renamed = FuncRenamer().visit(parsed) as! SourceFileSyntax
    let renamedSource = renamed.description
    expectEqual(renamedSource.count, 
      renamed.eofToken.positionAfterSkippingLeadingTrivia.utf8Offset)
    expectEqual(renamedSource.count, renamed.byteSize)
  })
}

PositionTests.test("CurrentFile") {
  expectDoesNotThrow({
    let parsed = try getSyntaxTree(URL(fileURLWithPath: #file))
    class Visitor: SyntaxVisitor {
      override func visitPre(_ node: Syntax) {
        _ = node.position
        _ = node.byteSize
        _ = node.positionAfterSkippingLeadingTrivia
      }
      override func visit(_ node: TokenSyntax) {
        expectEqual(node.positionAfterSkippingLeadingTrivia.utf8Offset,
          node.position.utf8Offset + node.leadingTrivia.byteSize)
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
            .withTrailingTrivia(.newlines(1)), expression: nil), semicolon: nil, errorTokens: nil))
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
    expectEqual(3, root.leadingTrivia!.count)
    expectEqual(0, root.trailingTrivia!.count)
    let state = root.statements[idx]
    expectEqual(3, state.leadingTrivia!.count)
    expectEqual(1, state.trailingTrivia!.count)
    expectEqual(state.byteSize,
      state.leadingTrivia!.byteSize + state.trailingTrivia!.byteSize
        + state.byteSizeAfterTrimmingTrivia)
    expectFalse(root.statements.isImplicit)
  })
}

PositionTests.test("Implicit") {
  expectDoesNotThrow({
    let root = createSourceFile(0)
    expectTrue(root.statements.isImplicit)
  })
}

PositionTests.test("WithoutSourceFileRoot") {
  expectDoesNotThrow({
    let item = SyntaxFactory.makeCodeBlockItem(
      item: SyntaxFactory.makeReturnStmt(
        returnKeyword: SyntaxFactory.makeToken(.returnKeyword, presence: .present)
          .withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1)),
          expression: nil), semicolon: nil, errorTokens: nil)
     expectEqual(0, item.position.utf8Offset)
     expectEqual(1, item.positionAfterSkippingLeadingTrivia.utf8Offset)
  })
}

runAllTests()
