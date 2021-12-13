// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-string-processing)

// REQUIRES: libswift,string_processing,executable_test

import StdlibUnittest

var RegexBasicTests = TestSuite("RegexBasic")

extension String {
  func expectMatch<T>(
    _ regex: Regex<T>,
    file: String = #file,
    line: UInt = #line
  ) -> RegexMatch<T> {
    guard let result = match(regex) else {
      expectUnreachable("Failed match", file: file, line: line)
      fatalError()
    }
    return result
  }
}

RegexBasicTests.test("Basic") {
  let input = "aabccd"

  let match1 = input.expectMatch('aabcc.')
  expectEqual("aabccd", input[match1.range])
  expectEqual(.empty, match1.captures)

  let match2 = input.expectMatch('a*b.+.')
  expectEqual("aabccd", input[match2.range])
  expectEqual(.empty, match2.captures)
}

RegexBasicTests.test("Captures") {
  let input = """
    A6F0..A6F1    ; Extend # Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM \
    COMBINING MARK TUKWENTIS
    """
  let regex = '([0-9A-F]+)(?:\.\.([0-9A-F]+))?\s+;\s+(\w+).*'
  let match1 = input.expectMatch(regex)
  expectEqual(input[...], input[match1.range])
  expectEqual(
    .tuple([
      .substring("A6F0"),
      .optional(.substring("A6F1")),
      .substring("Extend")
    ]),
    match1.captures)
}

runAllTests()
