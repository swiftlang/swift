// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -Xfrontend -enable-experimental-cxx-interop
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import CxxStdlib

func printString(s: std.string) {
    print(s)
    let swiftString = String(s)
    print(swiftString)
}

printString(s: "Hello")
// CHECK: basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>>()
// CHECK: Hello
