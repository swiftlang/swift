// RUN: not %swiftc_driver -emit-executable -o %t.exe %s -Xfrontend -debug-assert-immediately 2>&1 | FileCheck %s
// RUN: not %swiftc_driver -emit-executable -o %t.exe %s -Xfrontend -debug-assert-after-parse 2>&1 | FileCheck %s

// REQUIRES: asserts

// CHECK: error: swift frontend command failed due to signal

print("Hello, World")
