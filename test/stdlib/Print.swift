// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import StdlibUnittest
import PrintTestTypes


let PrintTests = TestSuite("Print")
PrintTests.test("Metatype") {
  expectPrinted("Int", Int.self)
  expectDebugPrinted("Swift.Int", Int.self)
}

PrintTests.test("StringInterpolation") {
  expectEqual("1", "\(1)")
  expectEqual("2", "\(1 + 1)")
  expectEqual("aaa1bbb2ccc", "aaa\(1)bbb\(2)ccc")
  
  expectEqual("1.0", "\(1.0)")
  expectEqual("1.5", "\(1.5)")
  expectEqual("1e-12", "\(1.0 / (1000000000000))")
  
  expectEqual("inf", "\(1 / 0.0)")
  expectEqual("-inf", "\(-1 / 0.0)")
  expectEqual("nan", "\(0 / 0.0)")
  
  expectEqual("<[►1◀︎, ►2◀︎, ►3◀︎]>", "<\([ StructPrintable(1), StructPrintable(2), StructPrintable(3) ])>")
  expectEqual("WithoutDescription(x: 1)", "\(WithoutDescription(1))")
}

PrintTests.test("StdoutUTF8") {
  expectPrinted("µ", "\u{00B5}")
}

PrintTests.test("Varargs") {
  var s0 = ""
  print("", 1, 2, 3, 4, "", separator: "|", to: &s0)
  expectEqual("|1|2|3|4|\n", s0)
  
  var s1 = ""
  print(1, 2, 3, separator: "\n", terminator: "===", to: &s1)
  expectEqual("1\n2\n3===", s1)
  
  var s2 = ""
  print(4, 5, 6, separator: "\n", to: &s2)
  expectEqual("4\n5\n6\n", s2)
  
  var s3 = ""
  print("", 1, 2, 3, 4, "", separator: "|", to: &s3)
  expectEqual("|1|2|3|4|\n", s3)
}

PrintTests.test("PlaygroundPrintHook") {
  var printed = ""
  _playgroundPrintHook = { printed = $0 }
  
  var s0 = ""
  print("", 1, 2, 3, 4, "", separator: "|", to: &s0)
  expectEqual("|1|2|3|4|\n", s0)
  print("%\(s0)%")
  expectEqual("%|1|2|3|4|\n%\n", printed)
  
  printed = ""
  var s1 = ""
  print("", 1, 2, 3, 4, "", separator: "!", to: &s1)
  expectEqual("", printed)
  print("%\(s1)%")
  expectEqual("%!1!2!3!4!\n%\n", printed)
}

runAllTests()
