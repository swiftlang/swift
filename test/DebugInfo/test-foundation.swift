// RUN: %target-swift-frontend -emit-ir -g %s -o %t.ll
// RUN: FileCheck %s --check-prefix SANITY < %t.ll
// RUN: FileCheck %s --check-prefix CHECK-HIDDEN < %t.ll
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
// SANITY: !DICompileUnit(

// CHECK-HIDDEN: @[[HIDDEN_GV:_TWVVSC.*]] = linkonce_odr hidden
// CHECK-HIDDEN-NOT: !DIGlobalVariable({{.*}}[[HIDDEN_GV]]

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
// IMPORT-CHECK: filename: "test-foundation.swift"
// IMPORT-CHECK: !MDModule(name: "ObjectiveC"
// IMPORT-CHECK: !DIDerivedType(tag: DW_TAG_member, name: "MyArr",
// IMPORT-CHECK-NOT:            line:
// IMPORT-CHECK-SAME:           baseType: ![[NSARRAY:"[^"]+"]]
// IMPORT-CHECK-SAME:           size: [[PTRSIZE]], align: [[PTRSIZE]]
// IMPORT-CHECK-NOT:            offset: 0
// IMPORT-CHECK-SAME:           ){{$}}
// IMPORT-CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "NSArray", scope: ![[FOUNDATION:[0-9]+]]
// IMPORT-CHECK-SAME:             identifier: [[NSARRAY]]
// IMPORT-CHECK: [[FOUNDATION]] = !MDModule(name: "Foundation",{{.*}} file: ![[FOUNDATION_FILE:[0-9]+]]
// IMPORT-CHECK: ![[FOUNDATION_FILE]] = !DIFile(filename: "Foundation-{{.*}}.pcm"
// IMPORT-CHECK: !DIImportedEntity(tag: DW_TAG_imported_module,{{.*}} entity: ![[FOUNDATION]]


  // Force the use of Int.
  var count : Int = 0

  func foo(obj: MyObject) {
    return obj.foo(obj)
  }
}

// SANITY-DAG: !DISubprogram(name: "blah",{{.*}} line: [[@LINE+2]],{{.*}} isDefinition: true
extension MyObject {
  func blah() {
    MyObject()
  }
}

// SANITY-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "NSObject",{{.*}} identifier: "_TtCSo8NSObject"
// SANITY-DAG: !DIGlobalVariable(name: "NsObj",{{.*}} line: [[@LINE+1]],{{.*}} type: !"_TtCSo8NSObject",{{.*}} isDefinition: true
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

// Make sure we build some witness tables for enums.
func useOptions(var opt: NSURLBookmarkCreationOptions)
       -> NSURLBookmarkCreationOptions {
  return [opt, opt]
}

// LOC-CHECK: ![[THUNK:.*]] = !DISubprogram({{.*}}linkageName: "_TToFC4main8MyObjectg5MyArrCSo7NSArray"
// LOC-CHECK-NOT:                           line:
// LOC-CHECK-SAME:                          isDefinition: true
// LOC-CHECK: ![[DBG]] = !DILocation(line: 0, scope: ![[THUNK]])

// These debug locations should all be in ordered by increasing line number.
// LOC-CHECK: ![[L1]] =
// LOC-CHECK: ![[L2]] =
// LOC-CHECK: ![[L3]] =
// LOC-CHECK: ![[L4]] =
