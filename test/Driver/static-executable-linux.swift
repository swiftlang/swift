// Build a static executable "hello world" program
// REQUIRES: OS=linux-gnu
// REQUIRES: static_stdlib
print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -static-executable -o %t/static-executable %s
// RUN: %t/static-executable | %FileCheck %s
// RUN: file %t/static-executable | %FileCheck %s --check-prefix=FILE
// CHECK: hello world!
// FILE: , statically linked,
