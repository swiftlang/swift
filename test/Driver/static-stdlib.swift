// Statically link a "hello world" program
// XFAIL: linux
// REQUIRES: static_stdlib
print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -static-stdlib -o %t/static-stdlib %s
// RUN: %target-run %t/static-stdlib | %FileCheck %s
// RUN: otool -L %t/static-stdlib | %FileCheck %s --check-prefix=OTOOL
// CHECK: hello world!
// OTOOL-NOT: libswiftCore.dylib 
