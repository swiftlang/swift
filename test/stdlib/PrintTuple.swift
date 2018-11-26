// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

import StdlibUnittest
import PrintTestTypes


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
  expectPrinted("[(1, \"two\", ►3◀︎, ►4◀︎, PrintTestTypes.WithoutDescription(x: 5))]",
    t0)
  
  let t1 = [ (1, "2", WithoutDescription(3)),
    (4, "5", WithoutDescription(6)),
    (7, "8", WithoutDescription(9)) ]
  expectPrinted("[(1, \"2\", PrintTestTypes.WithoutDescription(x: 3)), (4, \"5\", PrintTestTypes.WithoutDescription(x: 6)), (7, \"8\", PrintTestTypes.WithoutDescription(x: 9))]", t1)
}

runAllTests()
