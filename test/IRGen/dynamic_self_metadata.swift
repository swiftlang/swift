// RUN: %target-swift-frontend %use_no_opaque_pointers -disable-generic-metadata-prespecialization %s -emit-ir -parse-as-library | %FileCheck %s
// RUN: %target-swift-frontend -disable-generic-metadata-prespecialization %s -emit-ir -parse-as-library

// UNSUPPORTED: OS=windows-msvc
// REQUIRES: CPU=x86_64

// FIXME: Not a SIL test because we can't parse dynamic Self in SIL.
// <rdar://problem/16931299>

// CHECK: [[TYPE:%.+]] = type <{ [8 x i8] }>

@inline(never) func id<T>(_ t: T) -> T {
  return t
}
// CHECK-LABEL: define hidden swiftcc void @"$s21dynamic_self_metadata2idyxxlF"

protocol P {
  associatedtype T
}

extension P {
  func f() {}
}

struct G<T> : P {
  var t: T
}

class C {
  class func fromMetatype() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @"$s21dynamic_self_metadata1CC12fromMetatypeACXDSgyFZ"(%swift.type* swiftself %0)
  // CHECK: ret i64 0

  func fromInstance() -> Self? { return nil }
  // CHECK-LABEL: define hidden swiftcc i64 @"$s21dynamic_self_metadata1CC12fromInstanceACXDSgyF"(%T21dynamic_self_metadata1CC* swiftself %0)
  // CHECK: ret i64 0

  func dynamicSelfArgument() -> Self? {
    return id(nil)
  }
  // CHECK-LABEL: define hidden swiftcc i64 @"$s21dynamic_self_metadata1CC0A12SelfArgumentACXDSgyF"(%T21dynamic_self_metadata1CC* swiftself %0)
  // CHECK: [[GEP1:%.+]] = bitcast {{.*}} %0
  // CHECK: [[TYPE1:%.+]] = load {{.*}} [[GEP1]]
  // CHECK: [[T0:%.+]] = call swiftcc %swift.metadata_response @"$sSqMa"(i64 0, %swift.type* [[TYPE1]])
  // CHECK: [[TYPE2:%.+]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: call swiftcc void @"$s21dynamic_self_metadata2idyxxlF"({{.*}}, %swift.type* [[TYPE2]])

  func dynamicSelfConformingType() -> Self? {
    _ = G(t: self).f()
    return nil
  }
  // CHECK-LABEL: define hidden swiftcc i64 @"$s21dynamic_self_metadata1CC0A18SelfConformingTypeACXDSgyF"(%T21dynamic_self_metadata1CC* swiftself %0)
  // CHECK: [[SELF_GEP:%.+]] = bitcast {{.*}} %0
  // CHECK: [[SELF_TYPE:%.+]] = load {{.*}} [[SELF_GEP]]
  // CHECK: [[METADATA_RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$s21dynamic_self_metadata1GVMa"(i64 0, %swift.type* [[SELF_TYPE]])
  // CHECK: [[METADATA:%.*]] =  extractvalue %swift.metadata_response [[METADATA_RESPONSE]], 0
  // CHECK: call i8** @swift_getWitnessTable(%swift.protocol_conformance_descriptor* bitcast ({{.*}} @"$s21dynamic_self_metadata1GVyxGAA1PAAMc" to %swift.protocol_conformance_descriptor*), %swift.type* [[METADATA]], i8*** undef)
}
