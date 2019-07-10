// Statically link a "hello world" program
// XFAIL: linux, win32
// REQUIRES: static_stdlib
// REQUIRES: executable_test

print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -static-stdlib -o %t/static-stdlib %s
// RUN: %target-run %t/static-stdlib | %FileCheck %s
// RUN: otool -L %t/static-stdlib | %FileCheck %s --check-prefix=OTOOL
// CHECK: hello world!
// OTOOL-NOT: libswiftCore.dylib 
