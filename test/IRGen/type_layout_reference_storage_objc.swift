// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-ir %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// XFAIL: *

import Foundation

class NativeClass {}
class C: NSObject {}
@objc protocol P {}
@objc protocol Q {}
protocol NonObjC: class {}

// CHECK: @"$S34type_layout_reference_storage_objc26ReferenceStorageTypeLayoutVMn" = hidden constant {{.*}} @"$S34type_layout_reference_storage_objc26ReferenceStorageTypeLayoutVMP"
// CHECK: define internal %swift.type* @"$S34type_layout_reference_storage_objc26ReferenceStorageTypeLayoutVMi"
struct ReferenceStorageTypeLayout<T, ObjC: C> {
  var z: T

  // -- ObjC-refcounted class
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOXoWV", i32 8)
  unowned(safe)   var cs:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 8)
  unowned(unsafe) var cu:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var cwo: C?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var cwi: C!

  // -- ObjC-refcounted archetype
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOXoWV", i32 8)
  unowned(safe)   var os:  ObjC
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 8)
  unowned(unsafe) var ou:  ObjC
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var owo: ObjC?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var owi: ObjC!

  // -- Pure ObjC protocols are unknown-refcounted
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOXoWV", i32 8)
  unowned(safe)   var ps:  P
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 8)
  unowned(unsafe) var pu:  P
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var pwo: P?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var pwi: P!

  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOXoWV", i32 8)
  unowned(safe)   var pqs:  P & Q
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 8)
  unowned(unsafe) var pqu:  P & Q
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var pqwo: (P & Q)?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBOSgXwWV", i32 8)
  weak            var pqwi: (P & Q)!

  // -- Composition with ObjC protocol and native class is native-refcounted
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoXoWV", i32 8)
  unowned(safe)   var pncs:  (P & NativeClass)
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 8)
  unowned(unsafe) var pncu:  (P & NativeClass)
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 8)
  weak            var pncwo: (P & NativeClass)?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 8)
  weak            var pncwi: (P & NativeClass)!

  // -- Open-code layouts when there are witness tables.
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff_bt, i32 0, i32 0)
  unowned(safe)   var pqrs:  P & Q & NonObjC
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff_pod, i32 0, i32 0)
  unowned(unsafe) var pqru:  P & Q & NonObjC
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_1, i32 0, i32 0)
  weak            var pqrwo: (P & Q & NonObjC)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_1, i32 0, i32 0)
  weak            var pqrwi: (P & Q & NonObjC)!

  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff_bt, i32 0, i32 0)
  unowned(safe)   var pqrncs:  P & Q & NonObjC & NativeClass
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff_pod, i32 0, i32 0)
  unowned(unsafe) var pqrncu:  P & Q & NonObjC & NativeClass
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_1, i32 0, i32 0)
  weak            var pqrncwo: (P & Q & NonObjC & NativeClass)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_1, i32 0, i32 0)
  weak            var pqrncwi: (P & Q & NonObjC & NativeClass)!
}
