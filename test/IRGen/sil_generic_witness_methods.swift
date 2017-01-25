// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// FIXME: These should be SIL tests, but we can't parse generic types in SIL
// yet.

protocol P {
  func concrete_method()
  static func concrete_static_method()
  func generic_method<Z>(_ x: Z)
}

struct S {}

// CHECK-LABEL: define hidden swiftcc void @_T027sil_generic_witness_methods05call_D0{{[_0-9a-zA-Z]*}}F(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* %U, i8** %T.P)
func call_methods<T: P, U>(_ x: T, y: S, z: U) {
  // CHECK: [[STATIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.P, i32 1
  // CHECK: [[STATIC_METHOD_PTR:%.*]] = load i8*, i8** [[STATIC_METHOD_ADDR]], align 8
  // CHECK: [[STATIC_METHOD:%.*]] = bitcast i8* [[STATIC_METHOD_PTR]] to void (%swift.type*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[STATIC_METHOD]](%swift.type* swiftself %T, %swift.type* %T, i8** %T.P)
  T.concrete_static_method()

  // CHECK: [[CONCRETE_METHOD_PTR:%.*]] = load i8*, i8** %T.P, align 8
  // CHECK: [[CONCRETE_METHOD:%.*]] = bitcast i8* [[CONCRETE_METHOD_PTR]] to void (%swift.opaque*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[CONCRETE_METHOD]](%swift.opaque* noalias nocapture swiftself {{%.*}}, %swift.type* %T, i8** %T.P)
  x.concrete_method()
  // CHECK: [[GENERIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.P, i32 2
  // CHECK: [[GENERIC_METHOD_PTR:%.*]] = load i8*, i8** [[GENERIC_METHOD_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD:%.*]] = bitcast i8* [[GENERIC_METHOD_PTR]] to void (%swift.opaque*, %swift.type*, %swift.opaque*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[GENERIC_METHOD]](%swift.opaque* noalias nocapture {{.*}}, %swift.type* {{.*}} @_T027sil_generic_witness_methods1SVMf, {{.*}} %swift.opaque* noalias nocapture swiftself {{.*}}, %swift.type* %T, i8** %T.P)
  x.generic_method(y)
  // CHECK: [[GENERIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8*, i8** %T.P, i32 2
  // CHECK: [[GENERIC_METHOD_PTR:%.*]] = load i8*, i8** [[GENERIC_METHOD_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD:%.*]] = bitcast i8* [[GENERIC_METHOD_PTR]] to void (%swift.opaque*, %swift.type*, %swift.opaque*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[GENERIC_METHOD]](%swift.opaque* noalias nocapture {{.*}}, %swift.type* %U, %swift.opaque* noalias nocapture swiftself {{.*}}, %swift.type* %T, i8** %T.P)
  x.generic_method(z)
}

// CHECK-LABEL: define hidden swiftcc void @_T027sil_generic_witness_methods017call_existential_D0{{[_0-9a-zA-Z]*}}F(%P27sil_generic_witness_methods1P_* noalias nocapture dereferenceable({{.*}}))
func call_existential_methods(_ x: P, y: S) {
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %P27sil_generic_witness_methods1P_, %P27sil_generic_witness_methods1P_* [[X:%0]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]], align 8
  // CHECK: [[WTABLE_ADDR:%.*]] = getelementptr inbounds %P27sil_generic_witness_methods1P_, %P27sil_generic_witness_methods1P_* [[X]], i32 0, i32 2
  // CHECK: [[WTABLE:%.*]] = load i8**, i8*** [[WTABLE_ADDR]], align 8
  // CHECK: [[CONCRETE_METHOD_PTR:%.*]] = load i8*, i8** [[WTABLE]], align 8
  // CHECK: [[CONCRETE_METHOD:%.*]] = bitcast i8* [[CONCRETE_METHOD_PTR]] to void (%swift.opaque*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[CONCRETE_METHOD]](%swift.opaque* noalias nocapture swiftself {{%.*}}, %swift.type* [[METADATA]], i8** [[WTABLE]])
  x.concrete_method()

  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %P27sil_generic_witness_methods1P_, %P27sil_generic_witness_methods1P_* [[X]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]], align 8
  // CHECK: [[WTABLE_ADDR:%.*]] = getelementptr inbounds %P27sil_generic_witness_methods1P_, %P27sil_generic_witness_methods1P_* [[X:%.*]], i32 0, i32 2
  // CHECK: [[WTABLE:%.*]] = load i8**, i8*** [[WTABLE_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[WTABLE]], i32 2
  // CHECK: [[GENERIC_METHOD_PTR:%.*]] = load i8*, i8** [[GENERIC_METHOD_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD:%.*]] = bitcast i8* [[GENERIC_METHOD_PTR]] to void (%swift.opaque*, %swift.type*, %swift.opaque*, %swift.type*, i8**)*
  // CHECK: call swiftcc void [[GENERIC_METHOD]](%swift.opaque* noalias nocapture {{.*}}, %swift.type* {{.*}} @_T027sil_generic_witness_methods1SVMf, {{.*}} %swift.opaque* noalias nocapture swiftself {{%.*}}, %swift.type* [[METADATA]], i8** [[WTABLE]])
  x.generic_method(y)
}
