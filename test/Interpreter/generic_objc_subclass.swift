// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -Xfrontend -enable-dynamic-value-type-layout -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

// rdar://19583881

import Foundation
import ObjCClasses

class A<T> : HasHiddenIvars {
  var x: Int = 16
}

let a = A<Int>()

// CHECK: 0
// CHECK: 16
print(a.count)
print(a.x)

// CHECK: 25
// CHECK: 16
a.count = 25
print(a.count)
print(a.x)

// CHECK: 25
// CHECK: 36
a.x = 36
print(a.count)
print(a.x)

