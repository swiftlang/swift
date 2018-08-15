// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s -enable-objc-interop  | %FileCheck %s -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-objc-%target-ptrsize
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s -disable-objc-interop | %FileCheck %s -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-native-%target-ptrsize

class C {}
protocol P: class {}
protocol Q: class {}

// CHECK: @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMn" = hidden constant {{.*}} @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMP"
// CHECK: define internal %swift.type* @"$S29type_layout_reference_storage26ReferenceStorageTypeLayoutVMi"
struct ReferenceStorageTypeLayout<T, Native : C, Unknown : AnyObject> {
  var z: T

  // -- Known-Swift-refcounted type
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI:[0-9a-f][0-9a-f][0-9a-f]+]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI:[0-9a-f][0-9a-f][0-9a-f]+]]_pod, i32 0, i32 0)
  unowned(unsafe) var cu:  C
  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_1_bt, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_1_bt, i32 0, i32 0)
  unowned(safe)   var cs:  C
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var cwo: C?
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var cwi: C!

  // -- Known-Swift-refcounted archetype
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var nu:  Native
  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_1_bt, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_1_bt, i32 0, i32 0)
  unowned(safe)   var nc:  Native
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var nwo: Native?
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var nwi: Native!

  // -- Open-code layout for protocol types with witness tables. Note:
  //    1) The layouts for unowned(safe) references are only bitwise takable
  //       when ObjC interop is disabled.
  //    2) 0x7fffffff is the max extra inhabitant count, but not all types in
  //       all scenarios.
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var pu:  P
  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_7fffffff, i32 0, i32 0)
  unowned(safe)   var ps:  P
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_7fffffff, i32 0, i32 0)
  weak            var pwo: P?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_7fffffff, i32 0, i32 0)
  weak            var pwi: P!

  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff, i32 0, i32 0)
  unowned(safe)   var pqs:  P & Q
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var pqu:  P & Q
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff, i32 0, i32 0)
  weak            var pqwo: (P & Q)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff, i32 0, i32 0)
  weak            var pqwi: (P & Q)!

  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff_bt, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff_bt, i32 0, i32 0)
  unowned(safe)   var pqcs:  P & Q & C
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var pqcu:  P & Q & C
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff, i32 0, i32 0)
  weak            var pqcwo: (P & Q & C)?
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_24_8_7fffffff, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_12_4_7fffffff, i32 0, i32 0)
  weak            var pqcwi: (P & Q & C)!

  // -- Unknown-refcounted existential without witness tables.
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var aou:  AnyObject
  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_1, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_1, i32 0, i32 0)
  unowned(safe)   var aos:  AnyObject
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var aowo: AnyObject?
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var aowi: AnyObject!

  // -- Unknown-refcounted archetype
  // CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_pod, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_pod, i32 0, i32 0)
  unowned(unsafe) var uu:  Unknown
  // CHECK-native-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-64:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_8_1, i32 0, i32 0)
  // CHECK-native-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_[[REF_XI]]_bt, i32 0, i32 0)
  // CHECK-objc-32:   store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_4_4_1, i32 0, i32 0)
  unowned(safe)   var us:  Unknown
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var uwo: Unknown?
  // CHECK-64: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_8_8_0, i32 0, i32 0)
  // CHECK-32: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @type_layout_4_4_0, i32 0, i32 0)
  weak            var uwi: Unknown!
}


public class Base {
   var a: UInt32 = 0
}
// CHECK-LABEL: %swift.metadata_response @{{.*}}7DerivedCMr"(
// CHECK-64: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_16_8_{{.*}}_pod, i32 0, i32 0),
// CHECK-32: store i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @type_layout_8_4_{{.*}}_pod, i32 0, i32 0),
// CHECK: store i8** getelementptr inbounds (i8*, i8** @"$SBoWV", i32 8),
// CHECK: call swiftcc %swift.metadata_response @"$S29type_layout_reference_storage4BaseCMa"
// CHECK: call void @swift_initClassMetadata
// CHECK: ret
public class Derived<T> : Base {
  var type : P.Type
  var k = C()
  init(_ t: P.Type) {
    type = t
  }
}
