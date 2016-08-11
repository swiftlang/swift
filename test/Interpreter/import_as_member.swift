// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %clang -isysroot %sdk %S/Inputs/ImportAsMember/IAMVec.c -c -o %t/IAMVec.o
// RUN: %target-build-swift -I %S/Inputs/ImportAsMember/ -Xlinker %t/IAMVec.o %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_interpreter
// REQUIRES: OS=macosx

import ImportAsMember

var v3 = Vec3(x: 1.0, y: 2.0, z: 2.0)
print("Norm: \(v3.norm)") // CHECK: Norm: 3.0

// TODO: Test protocols

