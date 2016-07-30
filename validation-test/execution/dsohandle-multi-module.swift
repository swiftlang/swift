// RUN: rm -rf %t && mkdir %t

// RUN: (cd %t && %target-build-swift %S/Inputs/dsohandle-first.swift -emit-library -emit-module -module-name first)
// RUN: (cd %t && %target-build-swift %S/Inputs/dsohandle-second.swift -emit-library -emit-module -module-name second)
// RUN: %target-build-swift -I %t -L %t -lfirst -lsecond %s -o %t/main
// RUN: env LD_LIBRARY_PATH=%t DYLD_LIBRARY_PATH=%t %target-run %t/main
// REQUIRES: executable_test

// rdar://problem/27620565
// REQUIRES: OS=macosx

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
