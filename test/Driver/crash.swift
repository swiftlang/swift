// RUN: not %swiftc_driver -emit-executable -o %t.exe %s -Xfrontend -debug-crash-immediately 2>&1 | FileCheck %s

// RUN: not %swiftc_driver -emit-executable -o %t.exe %s -Xfrontend -debug-crash-after-parse 2>&1 | FileCheck %s

// CHECK: error: swift frontend command failed due to signal

func anchor() {}
anchor()

