// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import Darwin
import StdlibUnittest

protocol ProtocolUnrelatedToPrinting {}

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


let PrintTests = TestSuite("PrintClass")

PrintTests.test("ClassPrintable") {
  let c0 = ClassPrintable(1)
  let c1: ProtocolUnrelatedToPrinting = ClassPrintable(1)
  let c2: CustomStringConvertible = ClassPrintable(1)
  let c3: Any = ClassPrintable(1)
  
  expectPrinted("►1◀︎", c0)
  expectPrinted("►1◀︎", c1)
  expectPrinted("►1◀︎", c2)
  expectPrinted("►1◀︎", c3)
  
  let classMetatype = ClassPrintable.self
  expectPrinted("ClassPrintable", classMetatype)
  expectDebugPrinted("main.ClassPrintable", classMetatype)
  expectPrinted("[main.ClassPrintable]", [ classMetatype ])
  expectDebugPrinted("[main.ClassPrintable]", [ classMetatype ])
}

PrintTests.test("ClassVeryPrintable") {
  let c0 = ClassVeryPrintable(1)
  let c1: ProtocolUnrelatedToPrinting = ClassVeryPrintable(1)
  let c2: CustomStringConvertible = ClassVeryPrintable(1)
  let c3: CustomDebugStringConvertible = ClassVeryPrintable(1)
  let c4: Any = ClassVeryPrintable(1)
  
  expectPrinted("<description: 1>", c0)
  expectPrinted("<description: 1>", c1)
  expectPrinted("<description: 1>", c2)
  expectPrinted("<description: 1>", c3)
  expectPrinted("<description: 1>", c4)
}

runAllTests()
