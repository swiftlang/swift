// Statically link a "hello world" program
// REQUIRES: OS=linux-gnu
// REQUIRES: static_stdlib
print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -static-stdlib -use-ld=lld -o %t/static-stdlib-lld %s
// RUN: %t/static-stdlib-lld | %FileCheck %s
// RUN: ldd %t/static-stdlib-lld | %FileCheck %s --check-prefix=LDD
// CHECK: hello world!
// LDD-NOT: libswiftCore.so
