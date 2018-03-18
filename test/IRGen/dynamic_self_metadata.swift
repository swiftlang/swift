// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -parse-as-library | %FileCheck %s

// REQUIRES: CPU=x86_64

// FIXME: Not a SIL test because we can't parse dynamic Self in SIL.
// <rdar://problem/16931299>

// CHECK: [[TYPE:%.+]] = type <{ [8 x i8] }>

@inline(never) func id<T>(_ t: T) -> T {
  return t
}
// CHECK-LABEL: define hidden swiftcc void @"$S21dynamic_self_metadata2idyxxlF"

class C {
  class func fromMetatype() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @"$S21dynamic_self_metadata1CC12fromMetatypeACXDSgyFZ"(%swift.type* swiftself)
  // CHECK: ret i64 0

  func fromInstance() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @"$S21dynamic_self_metadata1CC12fromInstanceACXDSgyF"(%T21dynamic_self_metadata1CC* swiftself)
  // CHECK: ret i64 0

  func dynamicSelfArgument() -> Self? {
    return id(nil)
  }
  // CHECK-LABEL: define hidden swiftcc i64 @"$S21dynamic_self_metadata1CC0A12SelfArgumentACXDSgyF"(%T21dynamic_self_metadata1CC* swiftself)
  // CHECK: [[CAST1:%.+]] = bitcast %T21dynamic_self_metadata1CC* %0 to [[METATYPE:%.+]]
  // CHECK: [[TYPE1:%.+]] = call %swift.type* @swift_getObjectType([[METATYPE]] [[CAST1]])
  // CHECK: [[T0:%.+]] = call swiftcc %swift.metadata_response @"$SSqMa"(i64 0, %swift.type* [[TYPE1]])
  // CHECK: [[TYPE2:%.+]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: call swiftcc void @"$S21dynamic_self_metadata2idyxxlF"({{.*}}, %swift.type* [[TYPE2]])
}
