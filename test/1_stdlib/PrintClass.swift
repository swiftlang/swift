// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c  %S/Inputs/PrintTestTypes.swift -o %t/PrintTestTypes.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c  %S/Inputs/PrintTestTypes.swift -o %t/PrintTestTypes.o

// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import Swift
import StdlibUnittest
import PrintTestTypes

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
  expectDebugPrinted("PrintTestTypes.ClassPrintable", classMetatype)
  expectPrinted("[PrintTestTypes.ClassPrintable]", [ classMetatype ])
  expectDebugPrinted("[PrintTestTypes.ClassPrintable]", [ classMetatype ])
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
