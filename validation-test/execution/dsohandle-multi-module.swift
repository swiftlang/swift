// RUN: %empty-directory(%t)

// RUN: (cd %t && %target-build-swift %S/Inputs/dsohandle-first.swift -emit-library -emit-module -module-name first -Xlinker -install_name -Xlinker '@executable_path/libfirst.dylib')
// RUN: (cd %t && %target-build-swift %S/Inputs/dsohandle-second.swift -emit-library -emit-module -module-name second -Xlinker -install_name -Xlinker '@executable_path/libsecond.dylib')
// RUN: %target-build-swift -I %t -L %t -lfirst -lsecond %s -o %t/main
// RUN: %target-codesign %t/main %t/libfirst.%target-dylib-extension %t/libsecond.%target-dylib-extension
// RUN: %target-run %t/main %t/libfirst.%target-dylib-extension %t/libsecond.%target-dylib-extension

// REQUIRES: executable_test

// UNSUPPORTED: linux

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
