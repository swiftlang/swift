// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK

class C {}
protocol P: class {}
protocol Q: class {}

// CHECK: @_TMPV29type_layout_reference_storage26ReferenceStorageTypeLayout = hidden global {{.*}} @create_generic_metadata_ReferenceStorageTypeLayout
// CHECK: define private %swift.type* @create_generic_metadata_ReferenceStorageTypeLayout
struct ReferenceStorageTypeLayout<T> {
  var z: T

  // -- Known-Swift-refcounted type
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXoBo, i32 17)
  unowned(safe)   var cs:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVMBo, i32 17)
  unowned(unsafe) var cu:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXwGSqBo_, i32 17)
  weak            var cwo: C?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXwGSqBo_, i32 17)
  weak            var cwi: C!

  // -- Open-code layout for protocol types with witness tables.
  //   Note that the layouts for unowned(safe) references are
  //   only bitwise takable when ObjC interop is disabled.
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[UNOWNED_XI:[0-9a-f]+]]{{(,|_bt,)}} i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_[[UNOWNED_XI:[0-9a-f]+]]{{(,|_bt,)}} i32 0, i32 0)
  unowned(safe)   var ps:  P
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[REF_XI:[0-9a-f]+]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_[[REF_XI:[0-9a-f]+]]_pod, i32 0, i32 0)
  unowned(unsafe) var pu:  P
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[WEAK_XI:[0-9a-f]+]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_4_[[WEAK_XI:[0-9a-f]+]], i32 0, i32 0)
  weak            var pwo: P?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[WEAK_XI]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_4_[[WEAK_XI]], i32 0, i32 0)
  weak            var pwi: P!

  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[UNOWNED_XI]]{{(,|_bt,)}} i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[UNOWNED_XI]]{{(,|_bt,)}} i32 0, i32 0)
  unowned(safe)   var pqs:  P & Q
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var pqu:  P & Q
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[WEAK_XI]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_12_4_[[WEAK_XI]], i32 0, i32 0)
  weak            var pqwo: (P & Q)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[WEAK_XI]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_12_4_[[WEAK_XI]], i32 0, i32 0)
  weak            var pqwi: (P & Q)!

  // -- Unknown-refcounted existential without witness tables.
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXo[[UNKNOWN:B[Oo]]], i32 17)
  unowned(safe)   var aos:  AnyObject
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVMBo, i32 17)
  unowned(unsafe) var aou:  AnyObject
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXwGSq[[UNKNOWN]]_, i32 17)
  weak            var aowo: AnyObject?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @_TWVXwGSq[[UNKNOWN]]_, i32 17)
  weak            var aowi: AnyObject!
}
