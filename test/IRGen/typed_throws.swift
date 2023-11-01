// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-experimental-feature TypedThrows -disable-availability-checking -runtime-compatibility-version none -target %module-target-future | %FileCheck %s --check-prefix=CHECK-MANGLE

// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-experimental-feature TypedThrows -disable-availability-checking -runtime-compatibility-version 5.8 -disable-concrete-type-metadata-mangled-name-accessors | %FileCheck %s --check-prefix=CHECK-NOMANGLE

// XFAIL: CPU=arm64e

enum MyBigError: Error {
  case epicFail
}


// CHECK-MANGLE: @"$s12typed_throws1XVAA1PAAWP" = hidden global [2 x ptr] [ptr @"$s12typed_throws1XVAA1PAAMc", ptr getelementptr inbounds (i8, ptr @"symbolic ySi_____YKc 12typed_throws10MyBigErrorO", {{i32|i64}} 1)]
struct X: P {
  typealias A = (Int) throws(MyBigError) -> Void
}

func requiresP<T: P>(_: T.Type) { }
func createsP() {
  requiresP(X.self)
}

// CHECK-LABEL: define {{.*}}hidden swiftcc ptr @"$s12typed_throws13buildMetatypeypXpyF"()
func buildMetatype() -> Any.Type {
  typealias Fn = (Int) throws(MyBigError) -> Void

  // CHECK-MANGLE: __swift_instantiateConcreteTypeFromMangledName(ptr @"$sySi12typed_throws10MyBigErrorOYKcMD

  // CHECK-NOMANGLE: call swiftcc %swift.metadata_response @"$sySi12typed_throws10MyBigErrorOYKcMa"
  return Fn.self
}

// CHECK-NOMANGLE: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sySi12typed_throws10MyBigErrorOYKcMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata({{i32|i64}} 2231369729, {{i32|i64}} 0, ptr [[PARAMS:%.*]], ptr null, ptr getelementptr inbounds (%swift.full_existential_type, ptr @"$sytN", i32 0, i32 1), ptr null, i32 1, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr {{.*}}@"$s12typed_throws10MyBigErrorOMf"

protocol P {
  associatedtype A
}
