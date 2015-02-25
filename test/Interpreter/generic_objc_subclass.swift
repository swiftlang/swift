// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -Xfrontend -enable-dynamic-value-type-layout -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

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
println(a.count)
println(a.x)

// CHECK: 25
// CHECK: 16
a.count = 25
println(a.count)
println(a.x)

// CHECK: 25
// CHECK: 36
a.x = 36
println(a.count)
println(a.x)

