// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c  %S/Inputs/PrintTestTypes.swift -o %t/PrintTestTypes.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c  %S/Inputs/PrintTestTypes.swift -o %t/PrintTestTypes.o

// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import Swift
import StdlibUnittest
import PrintTestTypes

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
  let s = ("aaa\(1)bbb" as MyString).value
  expectEqual("<segment aaa><segment 1><segment bbb>", s)
}

runAllTests()
