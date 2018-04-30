// Statically link a "hello world" program
// SWIFT_ENABLE_TENSORFLOW: This test is unsupported because TensorFlow currently doesn't work with static-stdlib.
// UNSUPPORTED: tensorflow
// XFAIL: linux
// REQUIRES: static_stdlib
// REQUIRES: executable_test

print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -static-stdlib -o %t/static-stdlib %s
// RUN: %target-run %t/static-stdlib | %FileCheck %s
// RUN: otool -L %t/static-stdlib | %FileCheck %s --check-prefix=OTOOL
// CHECK: hello world!
// OTOOL-NOT: libswiftCore.dylib 
