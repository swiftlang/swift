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

var DecodeTests = TestSuite("DecodeSyntax")

DecodeTests.test("Basic") {
  expectDoesNotThrow({
    let content = try SwiftLang.parse(path: getInput("visitor.swift").path)
    let contentData = content.data(using: .utf8)!
    let source = try String(contentsOf: getInput("visitor.swift"))
    let parsed = try SyntaxTreeDeserializer().deserialize(contentData,
                     serializationFormat: .json)
    expectEqual("\(parsed)", source)
  })
}

runAllTests()
