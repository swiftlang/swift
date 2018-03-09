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
    expectEqual(parsed.eofToken.positionAfterSkippingLeadingTrivia.offset,
                source.count)
    expectEqual(parsed.byteSize, source.count)
  })
}

PositionTests.test("Closure") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(getInput("closure.swift"))
    let source = try String(contentsOf: getInput("closure.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    expectEqual(parsed.eofToken.positionAfterSkippingLeadingTrivia.offset,
                source.count)
    expectEqual(parsed.byteSize, source.count)
  })
}

PositionTests.test("Rename") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(getInput("visitor.swift"))
    let source = try String(contentsOf: getInput("visitor.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    let renamed = FuncRenamer().visit(parsed) as! SourceFileSyntax
    let renamedSource = renamed.description
    expectEqual(renamed.eofToken.positionAfterSkippingLeadingTrivia.offset,
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
    }
    Visitor().visit(parsed)
  })
}

runAllTests()
