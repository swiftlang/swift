// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

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
// CHECK-LABEL: @_T034witness_table_objc_associated_type2SBVAA1BAAWP = hidden constant [3 x i8*] [
// CHECK:         i8* bitcast (%swift.type* ()* @_T034witness_table_objc_associated_type2SAVMa to i8*)
// CHECK:         i8* bitcast (i8** ()* @_T034witness_table_objc_associated_type2SAVAA1AAAWa to i8*)
// CHECK:         i8* bitcast {{.*}} @_T034witness_table_objc_associated_type2SBVAA1BA2aDP3fooyyFTW
// CHECK:       ]

class CO: O {}
struct SO: C {
  typealias OO = CO
  func foo() {}
}
// CHECK-LABEL: @_T034witness_table_objc_associated_type2SOVAA1CAAWP = hidden constant [2 x i8*] [
// CHECK:         i8* bitcast (%swift.type* ()* @_T034witness_table_objc_associated_type2COCMa to i8*)
// CHECK:         i8* bitcast {{.*}} @_T034witness_table_objc_associated_type2SOVAA1CA2aDP3fooyyFTW
// CHECK:       ]

// CHECK-LABEL: define hidden swiftcc void @_T034witness_table_objc_associated_type0A25OffsetAfterAssociatedTypeyxAA1BRzlF(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.B)
func witnessOffsetAfterAssociatedType<T: B>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.B, i32 2
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call swiftcc void [[FOO]]
  x.foo()
}

// CHECK-LABEL: define hidden swiftcc void @_T034witness_table_objc_associated_type0A29OffsetAfterAssociatedTypeObjCyxAA1CRzlF(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.C) {{.*}} {
func witnessOffsetAfterAssociatedTypeObjC<T: C>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.C, i32 1
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call swiftcc void [[FOO]]
  x.foo()
}
