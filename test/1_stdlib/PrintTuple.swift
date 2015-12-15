// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// XFAIL: linux

import Swift
import Darwin
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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


struct WithoutDescription {
  let x: Int

  init(_ x: Int) {
    self.x = x
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

let PrintTests = TestSuite("PrintTuple")
PrintTests.test("Printable") {
  expectPrinted("(42, ())", (42, ()))
  expectPrinted("((), 42)", ((), 42))
  expectPrinted("(42, ►3◀︎)", (42, StructPrintable(3)))
  expectPrinted("(42, <10 20 30 40>)",
    (42, LargeStructPrintable(10, 20, 30, 40)))
  expectPrinted("(42, ►3◀︎)", (42, ClassPrintable(3)))
  expectPrinted("([123: 123], (1, 2, \"3\"))",
    ([123: 123], (1, 2, "3")))
  
  let t0 = [ (1, "two", StructPrintable(3), StructDebugPrintable(4),
    WithoutDescription(5)) ]
  expectPrinted("[(1, \"two\", ►3◀︎, ►4◀︎, main.WithoutDescription(x: 5))]",
    t0)
  
  let t1 = [ (1, "2", WithoutDescription(3)),
    (4, "5", WithoutDescription(6)),
    (7, "8", WithoutDescription(9)) ]
  expectPrinted("[(1, \"2\", main.WithoutDescription(x: 3)), (4, \"5\", main.WithoutDescription(x: 6)), (7, \"8\", main.WithoutDescription(x: 9))]", t1)
}

runAllTests()
