// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -whole-module-optimization -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test
// REQUIRES: reflection
// REQUIRES: stdlib_static_print

import StdlibUnittest

let PrintTests = TestSuite("StaticPrint")

// CHECK: string_literal utf8 ""
// CHECK: string_literal utf8 "hello"
// CHECK: string_literal utf8 "5"
// CHECK: string_literal utf8 "5"
// CHECK: string_literal utf8 ", world"
// CHECK: string_literal utf8 "hello, world 5"
PrintTests.test("StringInterpolation") {
  print("")
  print("hello")
  print("\(5)")
  let x = 5
  print("\(x)")
  let y = ", world"
  print("\(y)")
  print("hello \(y)\(x)")
}

runAllTests()
