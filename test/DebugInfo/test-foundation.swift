// REQUIRES: sdk
// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -triple x86_64-apple-darwin10 -emit-llvm -g %s -o %t.ll
// RUN: cat %t.ll | FileCheck %s --check-prefix SANITY
// RUN: cat %t.ll | FileCheck %s
// Sanity check for the regex below.
// SANITY: {{^ *ret }}
// SANITY: DW_TAG_compile_unit

// Make sure that there is no ret instruction without a !dbg annotation.
// This ensures that we associate every function with a debug scope.
// This test assumes that the '!' can only appear as part of a !dbg annotation.
// CHECK-NOT: {{^ *ret [^!]+$}}

import ObjectiveC
import Foundation

class MyObject : NSObject {
  func foo(obj: MyObject) {
    return obj.foo(obj);
  }
}

// FIXME (LLVM-branch): The [local] attribute means ObjectiveC-CC.
// SANITY: [ DW_TAG_subprogram ] [line [[@LINE+2]]] [local] [def] {{.*}}[blah]
extension MyObject {
  func blah() {
    println("blah blah blah")
  }
}

// SANITY-DAG: i32 [[@LINE+2]], metadata ![[TYPE:[0-9]+]]{{.*}} [ DW_TAG_variable ] [NsObj] [line [[@LINE+2]]] [def]
// SANITY-DAG: ![[TYPE]] = metadata{{.*}} [ DW_TAG_structure_type ] [_TtCSo8NSObject]
var NsObj: NSObject
NsObj = MyObject()
var MyObj: MyObject
MyObj = NsObj as! MyObject
MyObj.blah()

// CHECK: !"test-foundation.swift"
// CHECK: !"Foundation"
// CHECK: !"ObjectiveC"
