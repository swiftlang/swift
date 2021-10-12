// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Release -O
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/ReleasePrintDisabled -O -disable-print
// RUN: %target-run %t/Release | %FileCheck %s
// RUN: %target-run %t/ReleasePrintDisabled | %FileCheck %s --check-prefix CHECK-PRINT-DISABLED

// REQUIRES: executable_test

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif os(Windows)
import CRT
import WinSDK
#endif

puts("Start\n")
_customPrint("Hello world!")

// CHECK: Start
// CHECK: Hello world!

// CHECK-PRINT-DISABLED: Start
// CHECK-PRINT-DISABLED-NOT: Hello world!
