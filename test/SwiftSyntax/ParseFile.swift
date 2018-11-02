// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import SwiftSyntax
import SwiftLang

var ParseFile = TestSuite("ParseFile")

struct Foo {
  public let x: Int
  private(set) var y: [Bool]
}

#if os(macOS)
class Test: NSObject {
  @objc var bar: Int = 0
  func test() {
    print(#selector(function))
    print(#keyPath(bar))
  }
  @objc func function() {
  }
}
#endif

ParseFile.test("ParseSingleFile") {
  let currentFile = URL(fileURLWithPath: #file)
  expectDoesNotThrow({
    let currentFileContents = try String(contentsOf: currentFile)
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(try
      SwiftLang.parse(currentFile))
    expectEqual("\(parsed)", currentFileContents)
  })
}

ParseFile.test("ParseBuffer") {
  expectDoesNotThrow({
    let content = "func foo() {}"
    let parsed = try SourceFileSyntax.decodeSourceFileSyntax(try
      SwiftLang.parse(content))
    expectEqual("\(parsed)", content)
  })
}

runAllTests()
