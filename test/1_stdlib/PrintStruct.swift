// RUN: mkdir -p %t
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/a.out -Xlinker -dead_strip
// RUN: %target-run %t/a.out env %s
// RUN: %target-run %t/a.out ru_RU.UTF-8 %s
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

struct WithoutDescription {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }
}

struct EmptyStructWithoutDescription {}
protocol ProtocolUnrelatedToPrinting {}

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

struct StructPrintable : CustomStringConvertible, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
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


let PrintTests = TestSuite("PrintStruct")

PrintTests.test("Printable") {
  let s0 = [ WithoutDescription(1), WithoutDescription(2), WithoutDescription(3) ]
  expectPrinted(
  "[a.WithoutDescription(x: 1), a.WithoutDescription(x: 2), a.WithoutDescription(x: 3)]",
  s0)
  expectDebugPrinted(
  "[a.WithoutDescription(x: 1), a.WithoutDescription(x: 2), a.WithoutDescription(x: 3)]",
  s0)
  
  expectPrinted("EmptyStructWithoutDescription()",
    EmptyStructWithoutDescription())
  expectDebugPrinted("a.EmptyStructWithoutDescription()",
    EmptyStructWithoutDescription())
  
  expectPrinted(
    "ValuesWithoutDescription<Double, String, Array<Int>>(t: 1.25, u: \"abc\", v: [1, 2, 3])",
    ValuesWithoutDescription(1.25, "abc", [ 1, 2, 3 ]))
  expectDebugPrinted(
    "a.ValuesWithoutDescription<Swift.Double, Swift.String, Swift.Array<Swift.Int>>(t: 1.25, u: \"abc\", v: [1, 2, 3])", ValuesWithoutDescription(1.25, "abc", [ 1, 2, 3 ]))
}

PrintTests.test("custom string convertible structs") {
  struct Wrapper : CustomStringConvertible {
    var x: CustomStringConvertible? = nil
    
    var description: String {
      return "Wrapper(\(x.debugDescription))"
    }
  }

  expectPrinted("Wrapper(nil)", Wrapper())
  expectPrinted("Wrapper(Optional(Wrapper(nil)))",
    Wrapper(x: Wrapper()))
  expectPrinted("Wrapper(Optional(Wrapper(Optional(Wrapper(nil)))))",
    Wrapper(x: Wrapper(x: Wrapper())))
}

func test_ThickMetatypePrintingImpl<T>(
  thickMetatype: T.Type,
  _ expectedPrint: String,
  _ expectedDebug: String
  ) {
    expectPrinted(expectedPrint, thickMetatype)
    expectPrinted("[\(expectedDebug)]", [ thickMetatype ])
    expectDebugPrinted(expectedDebug, thickMetatype)
    expectDebugPrinted("[\(expectedDebug)]", [ thickMetatype ])
}

PrintTests.test("StructPrintable") {
  let s0 = StructPrintable(1)
  let s1: ProtocolUnrelatedToPrinting = StructPrintable(1)
  let s2: CustomStringConvertible = StructPrintable(1)
  let s3: Any = StructPrintable(1)
  
  expectPrinted("►1◀︎", s0)
  expectPrinted("►1◀︎", s1)
  expectPrinted("►1◀︎", s2)
  expectPrinted("►1◀︎", s3)
  
  let structMetatype = StructPrintable.self
  expectPrinted("StructPrintable", structMetatype)
  expectDebugPrinted("a.StructPrintable", structMetatype)
  expectPrinted("[a.StructPrintable]", [ structMetatype ])
  expectDebugPrinted("[a.StructPrintable]", [ structMetatype ])
  test_ThickMetatypePrintingImpl(structMetatype, "StructPrintable",
    "a.StructPrintable")
}

PrintTests.test("LargeStructPrintable") {
  let s0 = LargeStructPrintable(10, 20, 30, 40)
  let s1: ProtocolUnrelatedToPrinting = LargeStructPrintable(10, 20, 30, 40)
  let s2: CustomStringConvertible = LargeStructPrintable(10, 20, 30, 40)
  let s3: Any = LargeStructPrintable(10, 20, 30, 40)

  expectPrinted("<10 20 30 40>", s0)
  expectPrinted("<10 20 30 40>", s1)
  expectPrinted("<10 20 30 40>", s2)
  expectPrinted("<10 20 30 40>", s0)
  expectPrinted("<10 20 30 40>", s3)
}

PrintTests.test("StructVeryPrintable") {
  let s0 = StructVeryPrintable(1)
  let s1: ProtocolUnrelatedToPrinting = StructVeryPrintable(1)
  let s2: CustomStringConvertible = StructVeryPrintable(1)
  let s3: CustomDebugStringConvertible = StructVeryPrintable(1)
  let s4: Any = StructVeryPrintable(1)
  
  expectPrinted("<description: 1>", s0)
  expectPrinted("<description: 1>", s1)
  expectPrinted("<description: 1>", s2)
  expectPrinted("<description: 1>", s3)
  expectPrinted("<description: 1>", s4)
}

runAllTests()
