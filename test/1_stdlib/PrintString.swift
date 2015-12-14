// RUN: mkdir -p %t
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/a.out -Xlinker -dead_strip
// RUN: %target-run %t/a.out env %s
// RUN: %target-run %t/a.out ru_RU.UTF-8 %s
// REQUIRES: executable_test
// XFAIL: linux

import Swift
import Darwin
import StdlibUnittest

struct MyString : StringLiteralConvertible, StringInterpolationConvertible {
  init(str: String) {
    value = str
  }

  var value: String

  init(unicodeScalarLiteral value: String) {
    self.init(str: value)
  }

  init(extendedGraphemeClusterLiteral value: String) {
    self.init(str: value)
  }

  init(stringLiteral value: String) {
    self.init(str: value)
  }

  init(stringInterpolation strings: MyString...) {
    var result = ""
    for s in strings {
      result += s.value
    }
    self.init(str: result)
  }

  init<T>(stringInterpolationSegment expr: T) {
    self.init(str: "<segment " + String(expr) + ">")
  }
}

let PrintTests = TestSuite("PrintString")

PrintTests.test("Printable") {
  let s0: String = "abc"
  expectPrinted("abc", s0)
  expectDebugPrinted("\"abc\"", s0)

  let s1: String =  "\\ \' \" \0 \n \r \t \u{05}"
  expectDebugPrinted("\"\\\\ \\\' \\\" \\0 \\n \\r \\t \\u{05}\"", s1)

  let ch: Character = "a"
  expectPrinted("a", ch)
  expectDebugPrinted("\"a\"", ch)

  let us0: UnicodeScalar = "a"
  expectPrinted("a", us0)
  expectDebugPrinted("\"a\"", us0)

  let us1: UnicodeScalar = "\\"
  expectPrinted("\\", us1)
  expectEqual("\"\\\\\"", us1.description)
  expectDebugPrinted("\"\\\\\"", us1)

  let us2: UnicodeScalar = "あ"
  expectPrinted("あ", us2)
  expectEqual("\"あ\"", us2.description)
  expectDebugPrinted("\"\\u{3042}\"", us2)
}

PrintTests.test("Printable") {
  expectPrinted("nil", String!())
  expectPrinted("meow", String!("meow"))
  expectPrinted("nil", String?())
  expectPrinted("Optional(\"meow\")", String?("meow"))
}

PrintTests.test("CustomStringInterpolation") {
  expectEqual("<segment aaa><segment 1><segment bbb>",
    ("aaa\(1)bbb" as MyString).value)
}

runAllTests()
