// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import StdlibUnittest
import PrintTestTypes


let PrintTests = TestSuite("PrintArray")

PrintTests.test("Printable") {
  expectPrinted("[]", [Int]())
  expectPrinted("[1]", [ 1 ])
  expectPrinted("[1, 2]", [ 1, 2 ])
  expectPrinted("[1, 2, 3]", [ 1, 2, 3 ])
  
  expectPrinted("[\"foo\", \"bar\", \"bas\"]", ["foo", "bar", "bas"])
  expectDebugPrinted("[\"foo\", \"bar\", \"bas\"]", ["foo", "bar", "bas"])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [StructPrintable(1),
    StructPrintable(2), StructPrintable(3)])
  
  expectPrinted("[<10 20 30 40>, <50 60 70 80>]",
    [LargeStructPrintable(10, 20, 30, 40), LargeStructPrintable(50, 60, 70, 80)])
  
  expectPrinted("[►1◀︎]", [StructDebugPrintable(1)])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3)])
  
  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3)] as Array<AnyObject>)
}

runAllTests()
