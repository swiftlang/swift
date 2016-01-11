// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -o %t/main.out
// RUN: %target-run %t/main.out | FileCheck -check-prefix=CHECK-EMPTY %s
// RUN: %target-run %t/main.out --abc | FileCheck -check-prefix=CHECK-1 %s
// RUN: %target-run %t/main.out --abc def | FileCheck -check-prefix=CHECK-2 %s
// RUN: %target-run %t/main.out a --bcd efghijk | FileCheck -check-prefix=CHECK-3 %s
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

var CommandLineArguments = TestSuite("CommandLineArguments")
CommandLineArguments.test("printCommandLineArguments") {
  debugPrint(Process.arguments)
}
// CHECK-EMPTY: {{^}}out>>> ["{{[^"]+}}", "--stdlib-unittest-run-child"]{{$}}
// CHECK-1: {{^}}out>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "--abc"]{{$}}
// CHECK-2: {{^}}out>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "--abc", "def"]{{$}}
// CHECK-3: {{^}}out>>> ["{{[^"]+}}", "--stdlib-unittest-run-child", "a", "--bcd", "efghijk"]{{$}}

runAllTests()

