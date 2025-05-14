// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -whole-module-optimization -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift -swift-version 5 %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test
// REQUIRES: reflection

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
  expectEqual("\\", us1.description)
  expectDebugPrinted("\"\\\\\"", us1)

  let us2: UnicodeScalar = "あ"
  expectPrinted("あ", us2)
  expectEqual("あ", us2.description)
  expectDebugPrinted("\"\\u{3042}\"", us2)
}

PrintTests.test("TrickyQuoting") {
  guard #available(SwiftStdlib 5.9, *) else { return }
  // U+301: COMBINING ACUTE ACCENT (Grapheme_Cluster_Break = Extend)
  let s1 = "\u{301}Foo"
  expectPrinted(s1, s1)
  expectDebugPrinted("\"\\u{0301}Foo\"", s1)

  // U+302: COMBINING CIRCUMFLEX ACCENT (Grapheme_Cluster_Break = Extend)
  let s2 = "\u{301}\u{302}Foo"
  expectPrinted(s2, s2)
  expectDebugPrinted("\"\\u{0301}\\u{0302}Foo\"", s2)

  let s3 = "Foo\n\u{301}\u{302}Foo"
  expectPrinted(s3, s3)
  expectDebugPrinted("\"Foo\\n\\u{0301}\\u{0302}Foo\"", s3)

  // U+200D: ZERO WIDTH JOINER (Grapheme_Cluster_Break = ZWJ)
  let s4 = "\u{200d}Foo"
  expectPrinted(s4, s4)
  expectDebugPrinted("\"\\u{200D}Foo\"", s4)

  // U+110BD: KAITHI NUMBER SIGN (Grapheme_Cluster_Break = Prepend)
  let s5 = "Foo\u{110BD}"
  expectPrinted(s5, s5)
  expectDebugPrinted("\"Foo\\u{000110BD}\"", s5)

  // U+070F: SYRIAC ABBREVIATION MARK (Grapheme_Cluster_Break = Prepend)
  let s6 = "Foo\u{070F}\u{110BD}"
  expectPrinted(s6, s6)
  expectDebugPrinted("\"Foo\\u{070F}\\u{000110BD}\"", s6)

  let s7 = "Foo\u{301}\u{070F}\u{110BD}"
  expectPrinted(s7, s7)
  expectDebugPrinted("\"Foo\u{301}\\u{070F}\\u{000110BD}\"", s7)

  let s8 = "Foo\u{301}\u{302}\u{070F}\u{110BD}Foo"
  expectPrinted(s8, s8)
  expectDebugPrinted("\"Foo\u{0301}\u{0302}\u{070F}\u{110BD}Foo\"", s8)

  let s9 = "Foo\u{301}"
  expectPrinted(s9, s9)
  expectDebugPrinted("\"Foo\u{0301}\"", s9)
}

PrintTests.test("Optional") {
  expectPrinted("Optional(\"meow\")", String?("meow"))
}

PrintTests.test("StringInterpolation") {
  let s = "aaa\(1)bbb"
  expectEqual("aaa1bbb", s)
  
  let s2 = "aaa\(1)bbb\(2 as Any)"
  expectEqual("aaa1bbb2", s2)
  
  let x: Int? = 1
  let y: Int? = nil
  let s3 = "aaa\(x, default: "NONE")bbb\(y, default: "NONE")"
  expectEqual("aaa1bbbNONE", s3)
}

PrintTests.test("SubstringInterpolation") {
  let s = "aaa\(1)bbb" as Substring
  expectEqual("aaa1bbb", s)
  
  let s2 = "aaa\(1)bbb\(2 as Any)" as Substring
  expectEqual("aaa1bbb2", s2)
  
  let x: Int? = 1
  let y: Int? = nil
  let s3 = "aaa\(x, default: "NONE")bbb\(y, default: "NONE")" as Substring
  expectEqual("aaa1bbbNONE", s3)
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
  
  let x: Int? = 1
  let y: Int? = nil
  let s3 = ("aaa\(x, default: "NONE")bbb\(y, default: "NONE")" as MySimpleString).value
  expectEqual("aaa1bbbNONE", s3)
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
  
  let s5 = ("aaa\(required: true)bbb\(required: true, optional: true)ccc\(required: true, optional: false)ddd" as MyString).value
  expectEqual("12/3<literal aaa><interpolation:required:optional true false><literal bbb><interpolation:required:optional true true><literal ccc><interpolation:required:optional true false><literal ddd>", s5)
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
  
  public mutating
  func appendInterpolation(required: Bool, optional: Bool = false) {
    appendInterpolation(String(reflecting: required) + " " +
      String(reflecting: optional))
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
  
  let s5 = "aaa\(required: true)bbb\(required: true, optional: true)ccc\(required: true, optional: false)ddd"
  expectEqual("aaatrue falsebbbtrue trueccctrue falseddd", s5)
}

runAllTests()
