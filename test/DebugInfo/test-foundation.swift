// See <rdar://problem/16533351>
// RUN: rm -rf %t/clang-module-cache
// RUN: %target-build-swift -module-cache-path %t/clang-module-cache -emit-ir -g %s -o %t.ll
// RUN: cat %t.ll | FileCheck %s --check-prefix SANITY
// RUN: cat %t.ll | FileCheck %s --check-prefix IMPORT-CHECK
// RUN: cat %t.ll | FileCheck %s --check-prefix LOC-CHECK
// RUN: llc %t.ll -filetype=obj -o - |llvm-dwarfdump - | FileCheck %s --check-prefix DWARF-CHECK

// Make sure that there is no ret instruction without a !dbg annotation.
// This ensures that we associate every function with a debug scope.
//
// Unfortunately we need to exclude available_externally functions
// from this test; they are only there to help with optimizations and
// are ignored from MC on, so we don't emit debug info for them.
// FIXME: Come up with an invocation of clang-extract that can eliminate
// available_externally functions, so we can retire this hackfest:
// REQUIRES: OS=macosx
// RUN: cat %t.ll | grep -E '^define (available_externally|internal %%swift.type\*\*) ' | wc -l >%t.external_count
// This test assumes that the '!' can only appear as part of a !dbg annotation.
// RUN: cat %t.ll | grep -E '^ *ret [^!]+$' | count `cat %t.external_count`

// Sanity check for the regex above.
// SANITY: {{^ *ret }}
// SANITY: DW_TAG_compile_unit


import ObjectiveC
import Foundation

class MyObject : NSObject {
  // Ensure we don't emit linetable entries for ObjC thunks.
  // LOC-CHECK: define {{.*}} @_TToFC4main8MyObjectg5MyArrCSo7NSArray
  // LOC-CHECK: ret {{.*}}, !dbg ![[DBG:.*]]
  // LOC-CHECK: ret
  // LOC-CHECK: ![[THUNK:.*]] = {{.*}}metadata !"_TToFC4main8MyObjectg5MyArrCSo7NSArray", {{.*}} ; [ DW_TAG_subprogram ] [line [[@LINE+2]]] [local] [def]
  // LOC-CHECK: ![[DBG]] = metadata !{i32 0, i32 0, metadata ![[THUNK]], null}
  var MyArr = NSArray()
// Capture the pointer size from type Int
// IMPORT-CHECK: %Si = type <{ i[[PTRSIZE:[0-9]+]] }>
// IMPORT-CHECK: metadata ![[FOUNDATION:[0-9]+]], metadata !"NSArray", i32 {{.*}}, null, null, metadata ![[NSARRAY:.*]]} ; [ DW_TAG_structure_type ] [NSArray]
// IMPORT-CHECK: [[FOUNDATION]] = {{.*}} ; [ DW_TAG_module ] [Foundation]
// IMPORT-CHECK: metadata ![[NSARRAY]]} ; [ DW_TAG_member ] [MyArr] [line 0, size [[PTRSIZE]], align [[PTRSIZE]], offset 0] [from _TtCSo7NSArray]
// IMPORT-CHECK: metadata ![[FOUNDATION_FILE:[0-9]+]], metadata ![[FOUNDATION_MODULE:[0-9]+]], i32 1} ; [ DW_TAG_imported_module ]
// CHECK: ![[FOUNDATION_FILE]] = {{.*}}foundation.swift
// CHECK: ![[FOUNDATION_MODULE]] = {{.*}}[ DW_TAG_module ] [foundation]

  // Force the use of Int.
  var count : Int = 0

  func foo(obj: MyObject) {
    return obj.foo(obj)
  }
}

// FIXME (LLVM-branch): The [local] attribute means ObjectiveC-CC.
// SANITY-DAG: [ DW_TAG_subprogram ] [line [[@LINE+2]]] [local] [def] {{.*}}[blah]
extension MyObject {
  func blah() {
    println("blah blah blah")
  }
}

// SANITY-DAG: metadata !"_TtCSo8NSObject"} ; [ DW_TAG_structure_type ] [NSObject]
// SANITY-DAG: i32 [[@LINE+1]], metadata !"_TtCSo8NSObject", {{.*}} [ DW_TAG_variable ] [NsObj] [line [[@LINE+1]]] [def]
var NsObj: NSObject
NsObj = MyObject()
var MyObj: MyObject
MyObj = NsObj as MyObject
MyObj.blah()

func err() {
  // DWARF-CHECK: DW_AT_name{{.*}}NSError
  // DWARF-CHECK: DW_AT_linkage_name{{.*}}_TtCSo7NSError
  let error = NSError(domain: "myDomain", code: 4, 
                      userInfo: ["a":1,"b":2,"c":3])
}

// CHECK: !"test-foundation.swift"
// CHECK-DAG: !"Foundation"
// CHECK-DAG: !"ObjectiveC"
