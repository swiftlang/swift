// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import StdlibUnittest
import PrintTestTypes

let PrintTests = TestSuite("PrintCircularArray")

PrintTests.test("Printable") {
  expectPrinted("[]", CircularArray<Int>())
  expectPrinted("[1]", [ 1 ] as CircularArray)
  expectPrinted("[1, 2]", [ 1, 2 ] as CircularArray)
  expectPrinted("[1, 2, 3]", [ 1, 2, 3 ] as CircularArray)
  expectPrinted("[\"foo\", \"bar\", \"bas\"]", 
    ["foo", "bar", "bas"] as CircularArray)
  expectDebugPrinted("[\"foo\", \"bar\", \"bas\"]", 
    ["foo", "bar", "bas"] as CircularArray)

  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]",[StructPrintable(1),
    StructPrintable(2), StructPrintable(3)] as CircularArray)

  expectPrinted("[<10 20 30 40>, <50 60 70 80>]",
    [LargeStructPrintable(10, 20, 30, 40),
     LargeStructPrintable(50, 60, 70, 80)] as CircularArray)

  expectPrinted("[►1◀︎]", [StructDebugPrintable(1)] as CircularArray)

  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3)] as CircularArray)

  expectPrinted("[►1◀︎, ►2◀︎, ►3◀︎]", [ClassPrintable(1),
    ClassPrintable(2), ClassPrintable(3)] as CircularArray<AnyObject>)
}

runAllTests()
