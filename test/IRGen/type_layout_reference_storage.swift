// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK -DINT=i%target-ptrsize

class C {}
protocol P: class {}
protocol Q: class {}

// CHECK: @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMn" = hidden constant {{.*}} @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMP"
// CHECK: define internal %swift.type* @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMi"
struct ReferenceStorageTypeLayout<T, Native : C, Unknown : AnyObject> {
  var z: T

  // -- Known-Swift-refcounted type
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoXoWV", i32 9)
  unowned(safe)   var cs:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 9)
  unowned(unsafe) var cu:  C
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 9)
  weak            var cwo: C?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 9)
  weak            var cwi: C!

  // -- Known-Swift-refcounted archetype
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoXoWV", i32 9)
  unowned(safe)   var nc:  Native
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 9)
  unowned(unsafe) var nu:  Native
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 9)
  weak            var nwo: Native?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoSgXwWV", i32 9)
  weak            var nwi: Native!

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

  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[UNOWNED_XI]]{{(,|_bt,)}} i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[UNOWNED_XI]]{{(,|_bt,)}} i32 0, i32 0)
  unowned(safe)   var pqcs:  P & Q & C
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var pqcu:  P & Q & C
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[WEAK_XI]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_12_4_[[WEAK_XI]], i32 0, i32 0)
  weak            var pqcwo: (P & Q & C)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[WEAK_XI]], i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_12_4_[[WEAK_XI]], i32 0, i32 0)
  weak            var pqcwi: (P & Q & C)!

  // -- Unknown-refcounted existential without witness tables.
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN:B[Oo]]]XoWV", i32 9)
  unowned(safe)   var aos:  AnyObject
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 9)
  unowned(unsafe) var aou:  AnyObject
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN]]SgXwWV", i32 9)
  weak            var aowo: AnyObject?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN]]SgXwWV", i32 9)
  weak            var aowi: AnyObject!

  // -- Unknown-refcounted archetype
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN:B[Oo]]]XoWV", i32 9)
  unowned(safe)   var us:  Unknown
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBomWV", i32 9)
  unowned(unsafe) var uu:  Unknown
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN]]SgXwWV", i32 9)
  weak            var uwo: Unknown?
  // CHECK: store i8** getelementptr inbounds (i8*, i8** @"$S[[UNKNOWN]]SgXwWV", i32 9)
  weak            var uwi: Unknown!
}


public class Base {
   var a: UInt32 = 0
}
// CHECK-LABEL: %swift.type* @{{.*}}7DerivedCMi"(%swift.type_descriptor*, i8**, i8**)
// CHECK-NOT: store {{.*}}getelementptr{{.*}}SBomWV
// CHECK: call swiftcc %swift.metadata_response @"$S29type_layout_reference_storage1P_pXmTMa"([[INT]] 0)
// CHECK: store {{.*}}getelementptr{{.*}}SBoWV
// CHECK: ret
public class Derived<T> : Base {
  var type : P.Type
  var k = C()
  init(_ t: P.Type) {
    type = t
  }
}
