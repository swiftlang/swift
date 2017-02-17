// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -parse-as-library | %FileCheck %s

// REQUIRES: CPU=x86_64

// FIXME: Not a SIL test because we can't parse dynamic Self in SIL.
// <rdar://problem/16931299>

// CHECK: [[TYPE:%.+]] = type <{ [8 x i8] }>

@inline(never) func id<T>(_ t: T) -> T {
  return t
}
// CHECK-LABEL: define hidden swiftcc void @_TF21dynamic_self_metadata2idurFxx

class C {
  class func fromMetatype() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @_TZFC21dynamic_self_metadata1C12fromMetatypefT_GSqDS0__(%swift.type* swiftself)
  // CHECK: [[ALLOCA:%.+]] = alloca [[TYPE]], align 8
  // CHECK: [[CAST1:%.+]] = bitcast [[TYPE]]* [[ALLOCA]] to i64*
  // CHECK: store i64 0, i64* [[CAST1]], align 8
  // CHECK: [[CAST2:%.+]] = bitcast [[TYPE]]* [[ALLOCA]] to i64*
  // CHECK: [[LOAD:%.+]] = load i64, i64* [[CAST2]], align 8
  // CHECK: ret i64 [[LOAD]]

  func fromInstance() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @_TFC21dynamic_self_metadata1C12fromInstancefT_GSqDS0__(%C21dynamic_self_metadata1C* swiftself)
  // CHECK: [[ALLOCA:%.+]] = alloca [[TYPE]], align 8
  // CHECK: [[CAST1:%.+]] = bitcast [[TYPE]]* [[ALLOCA]] to i64*
  // CHECK: store i64 0, i64* [[CAST1]], align 8
  // CHECK: [[CAST2:%.+]] = bitcast [[TYPE]]* [[ALLOCA]] to i64*
  // CHECK: [[LOAD:%.+]] = load i64, i64* [[CAST2]], align 8
  // CHECK: ret i64 [[LOAD]]

  func dynamicSelfArgument() -> Self? {
    return id(nil)
  }
  // CHECK-LABEL: define hidden swiftcc i64 @_TFC21dynamic_self_metadata1C19dynamicSelfArgumentfT_GSqDS0__(%C21dynamic_self_metadata1C* swiftself)
  // CHECK: [[CAST1:%.+]] = bitcast %C21dynamic_self_metadata1C* %0 to [[METATYPE:%.+]]
  // CHECK: [[TYPE1:%.+]] = call %swift.type* @swift_getObjectType([[METATYPE]] [[CAST1]])
  // CHECK: [[TYPE2:%.+]] = call %swift.type* @_TMaSq(%swift.type* [[TYPE1]])
  // CHECK: call swiftcc void @_TF21dynamic_self_metadata2idurFxx({{.*}}, %swift.type* [[TYPE2]])
}
