// RUN: %empty-directory(%t)
// RUN: %target-clang -c --language=c %S/Inputs/forward-to-foobar.c -o %t/forward.o
// RUN: %target-build-swift %s %t/forward.o -Xfrontend -entry-point-function-name -Xfrontend foobar -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

// CHECK: howdy from foobar
print("howdy from foobar")

 
