// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: sdk_overlay

import Foundation
import StdlibUnittest
import SwiftSyntax

var ParseFile = TestSuite("ParseFile")

ParseFile.test("ParseSingleFile") {
  let currentFile = URL(fileURLWithPath: #file)
  expectDoesNotThrow({
    let currentFileContents = try String(contentsOf: currentFile)
    let parsed = try Syntax.parse(currentFile)
    expectEqual("\(parsed)", currentFileContents)
  })
}

runAllTests()
