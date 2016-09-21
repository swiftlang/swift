// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

protocol A {}

protocol B {
  associatedtype AA: A
  func foo()
}

@objc protocol O {}

protocol C {
  associatedtype OO: O
  func foo()
}

struct SA: A {}
struct SB: B {
  typealias AA = SA
  func foo() {}
}
// CHECK-LABEL: @_TWPV34witness_table_objc_associated_type2SBS_1BS_ = hidden constant [3 x i8*] [
// CHECK:         i8* bitcast (%swift.type* ()* @_TMaV34witness_table_objc_associated_type2SA to i8*)
// CHECK:         i8* bitcast (i8** ()* @_TWaV34witness_table_objc_associated_type2SAS_1AS_ to i8*)
// CHECK:         i8* bitcast {{.*}} @_TTWV34witness_table_objc_associated_type2SBS_1BS_FS1_3foofT_T_
// CHECK:       ]

class CO: O {}
struct SO: C {
  typealias OO = CO
  func foo() {}
}
// CHECK-LABEL: @_TWPV34witness_table_objc_associated_type2SOS_1CS_ = hidden constant [2 x i8*] [
// CHECK:         i8* bitcast (%swift.type* ()* @_TMaC34witness_table_objc_associated_type2CO to i8*)
// CHECK:         i8* bitcast {{.*}} @_TTWV34witness_table_objc_associated_type2SOS_1CS_FS1_3foofT_T_
// CHECK:       ]

// CHECK-LABEL: define hidden void @_TF34witness_table_objc_associated_type32witnessOffsetAfterAssociatedTypeuRxS_1BrFxT_(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.B)
func witnessOffsetAfterAssociatedType<T: B>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.B, i32 2
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call void [[FOO]]
  x.foo()
}

// CHECK-LABEL: define hidden void @_TF34witness_table_objc_associated_type36witnessOffsetAfterAssociatedTypeObjCuRxS_1CrFxT_(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.C) {{.*}} {
func witnessOffsetAfterAssociatedTypeObjC<T: C>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.C, i32 1
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call void [[FOO]]
  x.foo()
}
