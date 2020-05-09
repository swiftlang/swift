// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main.out
// RUN: %target-codesign %t/main.out
// RUN: %target-run %t/main.out | %FileCheck -check-prefix=CHECK-EMPTY %s
// RUN: %target-run %t/main.out --abc | %FileCheck -check-prefix=CHECK-1 %s
// RUN: %target-run %t/main.out --abc def | %FileCheck -check-prefix=CHECK-2 %s
// RUN: %target-run %t/main.out a --bcd efghijk | %FileCheck -check-prefix=CHECK-3 %s
// REQUIRES: executable_test

import StdlibUnittest


var CommandLineArguments = TestSuite("CommandLineArguments")
CommandLineArguments.test("printCommandLineArguments") {
  debugPrint(CommandLine.arguments)
}
// CHECK-EMPTY: {{^}}stdout>>> ["{{[^"]+}}", "--stdlib-unittest-run-child"]{{$}}
// CHECK-1: {{^}}stdout>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "--abc"]{{$}}
// CHECK-2: {{^}}stdout>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "--abc", "def"]{{$}}
// CHECK-3: {{^}}stdout>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "a", "--bcd", "efghijk"]{{$}}

runAllTests()

