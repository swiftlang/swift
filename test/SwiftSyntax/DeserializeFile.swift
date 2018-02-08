// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import SwiftSyntax
import SwiftSourceKit

func getInput(_ file: String) -> URL {
  var result = URL(fileURLWithPath: #file)
  result.deleteLastPathComponent()
  result.appendPathComponent("Inputs")
  result.appendPathComponent(file)
  return result
}

var DecodeTests = TestSuite("DecodeSyntax")

DecodeTests.test("Basic") {
  expectDoesNotThrow({
    let content = try SourceFileSyntax.encodeSourceFileSyntax(getInput("visitor.swift"))
    let source = try String(contentsOf: getInput("visitor.swift"))
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(content)
    expectEqual("\(parsed)", source)
  })
}

runAllTests()
