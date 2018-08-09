// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift -swift-version 5 %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

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
  expectPrinted("Optional(\"meow\")", String?("meow"))
}

PrintTests.test("StringInterpolation") {
  let s = "aaa\(1)bbb"
  expectEqual("aaa1bbb", s)
  
  let s2 = "aaa\(1)bbb\(2 as Any)"
  expectEqual("aaa1bbb2", s2)
}

PrintTests.test("SubstringInterpolation") {
  let s = "aaa\(1)bbb" as Substring
  expectEqual("aaa1bbb", s)
  
  let s2 = "aaa\(1)bbb\(2 as Any)" as Substring
  expectEqual("aaa1bbb2", s2)
}

PrintTests.test("CustomStringInterpolation") {
  let s = ("aaa\(1)bbb" as MyString).value
  expectEqual("6/1<literal aaa><interpolation:Int 1><literal bbb>", s)
  
  let s2 = ("aaa\(1)bbb\(2 as Any)" as MyString).value
  expectEqual("6/2<literal aaa><interpolation:Int 1><literal bbb><interpolation:T 2><literal >", s2)
}

PrintTests.test("AutoCustomStringInterpolation") {
  let s = ("aaa\(1)bbb" as MySimpleString).value
  expectEqual("aaa1bbb", s)
  
  let s2 = ("aaa\(1)bbb\(2 as Any)" as MySimpleString).value
  expectEqual("aaa1bbb2", s2)
}

PrintTests.test("CustomStringInterpolationExtra") {
  let s = ("aaa\(100)bbb\(100, radix: 16)ccc" as MyString).value
  expectEqual("9/2<literal aaa><interpolation:Int 100><literal bbb><interpolation:Int,radix 64><literal ccc>", s)
  
  let s2 = ("aaa\("X")bbb\(debug: "X")ccc" as MyString).value
  expectEqual("9/2<literal aaa><interpolation:T X><literal bbb><interpolation:T debug: \"X\"><literal ccc>", s2)
  
  let s3 = (try? "aaa\(fails: true)bbb" as MyString)?.value
  expectEqual(s3, nil)
  
  let s4 = (try? "aaa\(fails: false)bbb" as MyString)?.value
  expectEqual("6/1<literal aaa><interpolation:fails ><literal bbb>", s4)
}

extension DefaultStringInterpolation {
  mutating func appendInterpolation(_ expr: Int, radix: Int) {
    appendInterpolation(String(expr, radix: radix))
  }
  
  mutating func appendInterpolation<T>(debug expr: T) {
    appendInterpolation(String(reflecting: expr))
  }
  
  public mutating func appendInterpolation(fails: Bool) throws {
    if fails {
      throw MyStringError.failure
    }
    appendInterpolation("OK")
  }
}

PrintTests.test("StringInterpolationExtra") {
  let s = "aaa\(100)bbb\(100, radix: 16)ccc"
  expectEqual("aaa100bbb64ccc", s)
  
  let s2 = "aaa\("X")bbb\(debug: "X")ccc"
  expectEqual("aaaXbbb\"X\"ccc", s2)
  
  let s3 = try? "aaa\(fails: true)bbb"
  expectEqual(s3, nil)
  
  let s4 = try? "aaa\(fails: false)bbb"
  expectEqual("aaaOKbbb", s4)
}

runAllTests()
