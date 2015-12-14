// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import Darwin
import StdlibUnittest

protocol ProtocolUnrelatedToPrinting {}

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

class ClassPrintable : CustomStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "►\(x)◀︎"
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

let PrintTests = TestSuite("PrintArray")

PrintTests.test("Printable") {
  expectPrinted("[]", [Int]())
  expectPrinted("[1]", [ 1 ])
  expectPrinted("[1, 2]", [ 1, 2 ])
  expectPrinted("[1, 2, 3]", [ 1, 2, 3 ])
  
  expectPrinted("[\"foo\", \"bar\", \"bas\"]", [ "foo", "bar", "bas" ])
  expectDebugPrinted("[\"foo\", \"bar\", \"bas\"]", [ "foo", "bar", "bas" ])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ StructPrintable(1),
    StructPrintable(2), StructPrintable(3) ])
  
  expectPrinted("[<10 20 30 40>, <50 60 70 80>]",
    [ LargeStructPrintable(10, 20, 30, 40), LargeStructPrintable(50, 60, 70, 80) ])
  
  expectPrinted("[►1◀︎]", [ StructDebugPrintable(1) ])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3) ])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3) ] as Array<AnyObject>)
}

runAllTests()
