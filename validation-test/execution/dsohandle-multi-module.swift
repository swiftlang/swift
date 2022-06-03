// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(first)) %S/Inputs/dsohandle-first.swift -emit-module -module-name first
// RUN: %target-build-swift-dylib(%t/%target-library-name(second)) %S/Inputs/dsohandle-second.swift -emit-module -module-name second
// RUN: %target-build-swift -I %t -L %t -lfirst -lsecond %s -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(first) %t/%target-library-name(second)
// RUN: %target-run %t/main %t/%target-library-name(first) %t/%target-library-name(second)

// REQUIRES: executable_test

// UNSUPPORTED: OS=linux-gnu

import first
import second

import StdlibUnittest

let DSOHandleTests = TestSuite("DSOHandle")

DSOHandleTests.test("Unique handles for different images") {
  let firstHandle = getFirstDSOHandle()
  let secondHandle = getSecondDSOHandle()
  expectNotEqual(firstHandle, secondHandle)
}

runAllTests()
