// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -I %S/../Inputs/clang-importer-sdk/platform/any/usr/include %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test

import ctypes

var u = IntOrFloat()
print(u.i) // CHECK: 0
print(u.f) // CHECK: 0.0

u.i = 444
print(u.i) // CHECK: 444
u.f = 555.0
print(u.f) // CHECK: 555.0
print(u.i) // CHECK-NOT: 444

u = IntOrFloat(i: 777)
print(u.i) // CHECK: 777
print(u.f) // CHECK-NOT: 555.0

var s = StructWithNamedUnion()
print(s.a) // CHECK: 0
print(s.b) // CHECK: 0
print(s.intfloat.i) // CHECK: 0
print(s.intfloat.f) // CHECK: 0.0

s.a = 111
s.b = 222
s.intfloat = IntOrFloat(i: 333)
print(s.a) // CHECK: 111
print(s.b) // CHECK: 222
print(s.intfloat.i) // CHECK: 333
s.intfloat = IntOrFloat(f: 444.0)
print(s.intfloat.f) // CHECK: 444.0
print(s.intfloat.i) // CHECK-NOT: 333
