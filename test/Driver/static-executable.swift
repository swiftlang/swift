// Create a self contained binary
// REQUIRES: OS=linux-gnu
// REQUIRES: static_stdlib
print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -static-executable -o %t/static-executable %s
// RUN: %t/static-executable | %FileCheck %s
// RUN: %llvm-readelf -program-headers %t/static-executable | %FileCheck %s --check-prefix=ELF
// CHECK: hello world!
// ELF-NOT: INTERP
