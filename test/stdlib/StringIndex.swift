// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var StringIndexTests = TestSuite("StringIndexTests")

let simpleStrings = [
    "abcdefg", // Small ASCII
    "abéÏ", // Small Unicode
    "012345678901234567890", // Large ASCII
    "abéÏ012345678901234567890", // Large Unicode
]

func validateViewCount<View: BidirectionalCollection>(
  _ view: View, for string: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line) {

  var stackTrace = stackTrace.pushIf(showFrame, file: file, line: line)

  let count = view.count
  func expect(_ i: Int,
    file: String = #file, line: UInt = #line
  ) {
    expectEqual(count, i, "for String: \(string)",
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
      showFrame: false)
  }


  let reversedView = view.reversed()

  expect(Array(view).count)
  expect(view.indices.count)
  expect(view.indices.reversed().count)
  expect(reversedView.indices.count)
  expect(view.distance(from: view.startIndex, to: view.endIndex))
  expect(reversedView.distance(
    from: reversedView.startIndex, to: reversedView.endIndex))
}

StringIndexTests.test("views") {
  for s in simpleStrings {
    validateViewCount(s, for: s)
    validateViewCount(s.utf8, for: s)
    validateViewCount(s.utf16, for: s)
    validateViewCount(s.unicodeScalars, for: s)

    validateViewCount(s[...], for: s)
    validateViewCount(s[...].utf8, for: s)
    validateViewCount(s[...].utf16, for: s)
    validateViewCount(s[...].unicodeScalars, for: s)
  }
}

runAllTests()