// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import SwiftSyntax
import SwiftSourceKit

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
    let parsed = SourceFileSyntax.decodeSourceFileSyntax(try SourceKitdService.
      encodeSourceFileSyntax(currentFile))
    expectEqual("\(parsed)", currentFileContents)
  })
}

runAllTests()
