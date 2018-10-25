// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s -DINT=i%target-ptrsize

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
// CHECK-LABEL: @"$s34witness_table_objc_associated_type2SBVAA1BAAWP" = hidden global [4 x i8*] [
// CHECK:         i8* bitcast (i8** ()* @"$s34witness_table_objc_associated_type2SAVAcA1AAAWl
// CHECK:         @"symbolic 34witness_table_objc_associated_type2SAV"
// CHECK:         i8* bitcast {{.*}} @"$s34witness_table_objc_associated_type2SBVAA1BA2aDP3fooyyFTW"
// CHECK:       ]

class CO: O {}
struct SO: C {
  typealias OO = CO
  func foo() {}
}
// CHECK-LABEL: @"$s34witness_table_objc_associated_type2SOVAA1CAAWP" = hidden global [3 x i8*] [
// CHECK:         @"symbolic 34witness_table_objc_associated_type2COC"
// CHECK:         i8* bitcast {{.*}} @"$s34witness_table_objc_associated_type2SOVAA1CA2aDP3fooyyFTW"
// CHECK:       ]

// CHECK-LABEL: define hidden swiftcc void @"$s34witness_table_objc_associated_type0A25OffsetAfterAssociatedTypeyyxAA1BRzlF"(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.B)
func witnessOffsetAfterAssociatedType<T: B>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.B, i32 3
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call swiftcc void [[FOO]]
  x.foo()
}

// CHECK-LABEL: define hidden swiftcc void @"$s34witness_table_objc_associated_type0A29OffsetAfterAssociatedTypeObjCyyxAA1CRzlF"(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.C) {{.*}} {
func witnessOffsetAfterAssociatedTypeObjC<T: C>(_ x: T) {
  // CHECK:         [[FOO_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.C, i32 2
  // CHECK:         [[FOO_OPAQUE:%.*]] = load {{.*}} [[FOO_ADDR]]
  // CHECK:         [[FOO:%.*]] = bitcast {{.*}} [[FOO_OPAQUE]]
  // CHECK:         call swiftcc void [[FOO]]
  x.foo()
}
