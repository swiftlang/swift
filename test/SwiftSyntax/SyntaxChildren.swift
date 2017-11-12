// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: sdk_overlay

import Foundation
import StdlibUnittest
import SwiftSyntax

/// Verifies that there is a next item returned by the iterator and that it
/// satisfies the given predicate.
func expectNext<Iterator: IteratorProtocol>(
  _ iterator: inout Iterator,
  satisfies predicate: (Iterator.Element) throws -> Bool
) rethrows {
  let next = iterator.next()
  expectNotNil(next)
  expectTrue(try predicate(next!))
}

/// Verifies that the iterator is exhausted.
func expectNextIsNil<Iterator: IteratorProtocol>(_ iterator: inout Iterator) {
  expectNil(iterator.next())
}

var SyntaxChildrenAPI = TestSuite("SyntaxChildrenAPI")

SyntaxChildrenAPI.test("IterateWithAllPresent") {
  let returnStmt = SyntaxFactory.makeReturnStmt(
    returnKeyword: SyntaxFactory.makeReturnKeyword(),
    expression: SyntaxFactory.makeBlankExpr(),
    semicolon: SyntaxFactory.makeSemicolonToken())

  var iterator = returnStmt.children.makeIterator()
  expectNext(&iterator) { ($0 as? TokenSyntax)?.tokenKind == .returnKeyword }
  expectNext(&iterator) { $0 is ExprSyntax }
  expectNext(&iterator) { ($0 as? TokenSyntax)?.tokenKind == .semicolon }
  expectNextIsNil(&iterator)
}

SyntaxChildrenAPI.test("IterateWithSomeMissing") {
  let returnStmt = SyntaxFactory.makeReturnStmt(
    returnKeyword: SyntaxFactory.makeReturnKeyword(),
    expression: nil,
    semicolon: SyntaxFactory.makeSemicolonToken())

  var iterator = returnStmt.children.makeIterator()
  expectNext(&iterator) { ($0 as? TokenSyntax)?.tokenKind == .returnKeyword }
  expectNext(&iterator) { ($0 as? TokenSyntax)?.tokenKind == .semicolon }
  expectNextIsNil(&iterator)
}

SyntaxChildrenAPI.test("IterateWithAllMissing") {
  let returnStmt = SyntaxFactory.makeBlankReturnStmt()

  var iterator = returnStmt.children.makeIterator()
  expectNextIsNil(&iterator)
}

runAllTests()
