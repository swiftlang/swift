// RUN: %empty-directory(%t)
// RUN: %target-clang -fobjc-arc %S/Inputs/objc_implicit_inner_pointer/objc_implicit_inner_pointer.m -c -o %t/objc_implicit_inner_pointer.o
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/objc_implicit_inner_pointer/objc_implicit_inner_pointer.h %t/objc_implicit_inner_pointer.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

do {
  // The lifetime of Foo() currently gets extended using autorelease.
  autoreleasepool {
    let x = Foo().bar().takeUnretainedValue()
    print(x) // CHECK: 1234567891
  } // CHECK: -[Foo dealloc]

  autoreleasepool {
    let y = Foo().nullabar()!.takeUnretainedValue()
    print(y) // CHECK: 1234567891
  } // CHECK: -[Foo dealloc]
}

