// RUN: %target-swift-frontend -emit-ir -g %s -o %t.ll
// RUN: FileCheck %s --check-prefix SANITY < %t.ll
// RUN: FileCheck %s --check-prefix IMPORT-CHECK < %t.ll
// RUN: FileCheck %s --check-prefix LOC-CHECK < %t.ll
// RUN: llc %t.ll -filetype=obj -o %t.o
// RUN: llvm-dwarfdump %t.o | FileCheck %s --check-prefix DWARF-CHECK
// RUN: dwarfdump --verify %t.o

// Make sure that there is no ret instruction without a !dbg annotation.
// This ensures that we associate every function with a debug scope.
//
// Unfortunately we need to exclude available_externally functions
// from this test; they are only there to help with optimizations and
// are ignored from MC on, so we don't emit debug info for them.
// FIXME: Come up with an invocation of clang-extract that can eliminate
// available_externally functions, so we can retire this hackfest:
// REQUIRES: OS=macosx
// RUN: cat %t.ll | grep -E '^define .*(\@get_field_types|\@_TMa)' | wc -l >%t.external_count
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
  var MyArr = NSArray()
// Capture the pointer size from type Int
// IMPORT-CHECK: %Si = type <{ i[[PTRSIZE:[0-9]+]] }>
// IMPORT-CHECK-DAG: ![[FOUNDATION:[0-9]+]], {{[^,]+}}, {{[^,]+}}, null, null, ![[NSARRAY:.*]]} ; [ DW_TAG_structure_type ] [NSArray]
// IMPORT-CHECK-DAG: [[FOUNDATION]] = {{.*}}![[FOUNDATION_FILE:[0-9]+]], null} ; [ DW_TAG_module ] [Foundation]
// IMPORT-CHECK-DAG: ![[NSARRAY]]} ; [ DW_TAG_member ] [MyArr] [line 0, size [[PTRSIZE]], align [[PTRSIZE]], offset 0] [from _TtCSo7NSArray]
// IMPORT-CHECK-DAG: ![[FOUNDATION]]} ; [ DW_TAG_imported_module ]
// IMPORT-CHECK-DAG: ![[FOUNDATION_FILE]] = {{.*}}Foundation

  // Force the use of Int.
  var count : Int = 0

  func foo(obj: MyObject) {
    return obj.foo(obj)
  }
}

// FIXME (LLVM-branch): The [local] attribute means ObjectiveC-CC.
// SANITY-DAG: [ DW_TAG_subprogram ] [line [[@LINE+2]]] [def] {{.*}}[blah]
extension MyObject {
  func blah() {
    println("blah blah blah")
  }
}

// SANITY-DAG: !"_TtCSo8NSObject"} ; [ DW_TAG_structure_type ] [NSObject]
// SANITY-DAG: !"_TtCSo8NSObject", {{.*}} [ DW_TAG_variable ] [NsObj] [line [[@LINE+1]]] [def]
var NsObj: NSObject
NsObj = MyObject()
var MyObj: MyObject
MyObj = NsObj as! MyObject
MyObj.blah()

public func err() {
  // DWARF-CHECK: DW_AT_name{{.*}}NSError
  // DWARF-CHECK: DW_AT_linkage_name{{.*}}_TtCSo7NSError
  let error = NSError(domain: "myDomain", code: 4, 
                      userInfo: ["a":1,"b":2,"c":3])
}

// LOC-CHECK: define {{.*}}4date
public func date() {
  // LOC-CHECK: call {{.*}} @_TFSSCfMSSFT21_builtinStringLiteralBp8byteSizeBw7isASCIIBi1__SS{{.*}}, !dbg ![[L1:.*]]
  let d1 = NSDateFormatter()
  // LOC-CHECK: br{{.*}}, !dbg ![[L2:.*]]
  d1.dateFormat = "dd. mm. yyyy" // LOC-CHECK: call{{.*}}objc_msgSend{{.*}}, !dbg ![[L2]]
  // LOC-CHECK: call {{.*}} @_TFSSCfMSSFT21_builtinStringLiteralBp8byteSizeBw7isASCIIBi1__SS{{.*}}, !dbg ![[L3:.*]]
  let d2 = NSDateFormatter()
  // LOC-CHECK: br{{.*}}, !dbg ![[L4:.*]]
  d2.dateFormat = "mm dd yyyy" // LOC-CHECK: call{{.*}}objc_msgSend{{.*}}, !dbg ![[L4]]
}

// IMPORT-CHECK-DAG: !"test-foundation.swift"
// IMPORT-CHECK-DAG: [ DW_TAG_module ] [ObjectiveC]

// LOC-CHECK: ![[THUNK:.*]] = {{.*}}_TToFC4main8MyObjectg5MyArrCSo7NSArray{{.*}} ; [ DW_TAG_subprogram ] [line 0] [def]
// LOC-CHECK: ![[DBG]] = !MDLocation(line: 0, scope: ![[THUNK]])

// These debug locations should all be in ordered by increasing line number.
// LOC-CHECK: ![[L1]] =
// LOC-CHECK: ![[L2]] =
// LOC-CHECK: ![[L3]] =
// LOC-CHECK: ![[L4]] =
