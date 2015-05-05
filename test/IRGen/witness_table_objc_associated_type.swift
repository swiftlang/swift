// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

protocol A {}

protocol B {
  typealias AA: A
  func foo()
}

@objc protocol O {}

protocol C {
  typealias OO: O
  func foo()
}

struct SA: A {}
struct SB: B {
  typealias AA = SA
  func foo() {}
}
// CHECK-LABEL: @_TWPV34witness_table_objc_associated_type2SBS_1BS_ = hidden constant [3 x i8*] [
// CHECK:         i8* null
// CHECK:         i8* null
// CHECK:         i8* bitcast {{.*}} @_TTWV34witness_table_objc_associated_type2SBS_1BS_FS1_3fooUS1__US_1A__fQPS1_FT_T_
// CHECK:       ]

class CO: O {}
struct SO: C {
  typealias OO = CO
  func foo() {}
}
// CHECK-LABEL: @_TWPV34witness_table_objc_associated_type2SOS_1CS_ = hidden constant [2 x i8*] [
// CHECK:         i8* null
// CHECK:         i8* bitcast {{.*}} @_TTWV34witness_table_objc_associated_type2SOS_1CS_FS1_3fooUS1__US_1O__fQPS1_FT_T_
// CHECK:       ]

// CHECK-LABEL: define hidden void @_TF34witness_table_objc_associated_type32witnessOffsetAfterAssociatedTypeUS_1B_US_1A__FQ_T_(%swift.opaque*, %swift.type* %T, i8** %T.B, %swift.type* %T.AA, i8** %T.AA.A)
func witnessOffsetAfterAssociatedType<T: B>(x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.B, i32 2
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call void [[FOO]]
  x.foo()
}

// CHECK-LABEL: define hidden void @_TF34witness_table_objc_associated_type36witnessOffsetAfterAssociatedTypeObjCUS_1C_US_1O__FQ_T_(%swift.opaque*, %swift.type* %T, i8** %T.C, %swift.type* %T.OO) {{.*}} {
func witnessOffsetAfterAssociatedTypeObjC<T: C>(x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.C, i32 1
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call void [[FOO]]
  x.foo()
}
