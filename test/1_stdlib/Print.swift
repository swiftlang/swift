// RUN: mkdir -p %t
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/a.out -Xlinker -dead_strip
// RUN: %target-run %t/a.out env %s
// RUN: %target-run %t/a.out ru_RU.UTF-8 %s
// REQUIRES: executable_test
// XFAIL: linux

import Swift
import Darwin
import StdlibUnittest

let PrintTests = TestSuite("Print")

let arg = Process.arguments[1]
if arg == "env" {
  setlocale(LC_ALL, "")
} else {
  expectEqual("ru_RU.UTF-8", arg)
  setlocale(LC_ALL, arg)
}

protocol ProtocolUnrelatedToPrinting {}

struct WithoutDescription {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }
}

struct StructPrintable : CustomStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "►\(x)◀︎"
  }
}

struct LargeStructPrintable : CustomStringConvertible, ProtocolUnrelatedToPrinting {
  let a: Int
  let b: Int
  let c: Int
  let d: Int

  init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }

  var description: String {
    return "<\(a) \(b) \(c) \(d)>"
  }
}

struct StructDebugPrintable : CustomDebugStringConvertible {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var debugDescription: String {
    return "►\(x)◀︎"
  }
}

struct StructVeryPrintable : CustomStringConvertible, CustomDebugStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "<description: \(x)>"
  }

  var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

struct EmptyStructWithoutDescription {}

struct ValuesWithoutDescription<T, U, V> {
  let t: T
  let u: U
  let v: V

  init(_ t: T, _ u: U, _ v: V) {
    self.t = t
    self.u = u
    self.v = v
  }
}


class ClassPrintable : CustomStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "►\(x)◀︎"
  }
}

class ClassVeryPrintable : CustomStringConvertible, CustomDebugStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "<description: \(x)>"
  }

  var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

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
  print("", 1, 2, 3, 4, "", separator: "|", toStream: &s0)
  expectEqual("|1|2|3|4|\n", s0)
  
  var s1 = ""
  print(1, 2, 3, separator: "\n", terminator: "===", toStream: &s1)
  expectEqual("1\n2\n3===", s1)
  
  var s2 = ""
  print(4, 5, 6, separator: "\n", toStream: &s2)
  expectEqual("4\n5\n6\n", s2)
  
  var s3 = ""
  print("", 1, 2, 3, 4, "", separator: "|", toStream: &s3)
  expectEqual("|1|2|3|4|\n", s3)
}

PrintTests.test("PlaygroundPrintHook") {
  var printed = ""
  _playgroundPrintHook = { printed = $0 }
  
  var s0 = ""
  print("", 1, 2, 3, 4, "", separator: "|", toStream: &s0)
  expectEqual("|1|2|3|4|\n", s0)
  print("%\(s0)%")
  expectEqual("%|1|2|3|4|\n%\n", printed)
  
  printed = ""
  var s1 = ""
  print("", 1, 2, 3, 4, "", separator: "!", toStream: &s1)
  expectEqual("", printed)
  print("%\(s1)%")
  expectEqual("%!1!2!3!4!\n%\n", printed)
}

runAllTests()
