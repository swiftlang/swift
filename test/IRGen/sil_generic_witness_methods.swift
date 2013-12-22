// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -emit-sil-protocol-witness-tables | FileCheck %s

// FIXME: These should be SIL tests, but we can't parse generic types in SIL
// yet.

protocol P {
  func concrete_method()
  static func concrete_static_method()
  func generic_method<Z>(x: Z)
}

struct S {}

// CHECK: define void @_TF27sil_generic_witness_methods12call_methodsUS_1P___FT1xQ_1yVS_1S1zQ0__T_(%swift.opaque* noalias, %swift.opaque* noalias, %swift.type* %T, i8** %T.P, %swift.type* %U)
func call_methods<T: P, U>(x: T, y: S, z: U) {
  // CHECK: [[STATIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8** %T.P, i32 1
  // CHECK: [[STATIC_METHOD_PTR:%.*]] = load i8** [[STATIC_METHOD_ADDR]], align 8
  // CHECK: [[STATIC_METHOD:%.*]] = bitcast i8* [[STATIC_METHOD_PTR]] to void (%swift.type*, %swift.type*)*
  // CHECK: call void [[STATIC_METHOD]](%swift.type* %T, %swift.type* %T)
  T.concrete_static_method()

  // CHECK: [[CONCRETE_METHOD_PTR:%.*]] = load i8** %T.P, align 8
  // CHECK: [[CONCRETE_METHOD:%.*]] = bitcast i8* [[CONCRETE_METHOD_PTR]] to void (%swift.opaque*, %swift.type*)*
  // CHECK: call void [[CONCRETE_METHOD]](%swift.opaque* {{%.*}}, %swift.type* %T)
  x.concrete_method()
  // CHECK: [[GENERIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8** %T.P, i32 2
  // CHECK: [[GENERIC_METHOD_PTR:%.*]] = load i8** [[GENERIC_METHOD_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD:%.*]] = bitcast i8* [[GENERIC_METHOD_PTR]] to void (%swift.opaque*, %swift.opaque*, %swift.type*, %swift.type*)*
  // CHECK: call void [[GENERIC_METHOD]](%swift.opaque* {{.*}}, %swift.opaque* {{.*}}, %swift.type* getelementptr inbounds ({{.*}} @_TMdV27sil_generic_witness_methods1S {{.*}}), %swift.type* %T)
  x.generic_method(y)
  // CHECK: [[GENERIC_METHOD_ADDR:%.*]] = getelementptr inbounds i8** %T.P, i32 2
  // CHECK: [[GENERIC_METHOD_PTR:%.*]] = load i8** [[GENERIC_METHOD_ADDR]], align 8
  // CHECK: [[GENERIC_METHOD:%.*]] = bitcast i8* [[GENERIC_METHOD_PTR]] to void (%swift.opaque*, %swift.opaque*, %swift.type*, %swift.type*)*
  // CHECK: call void [[GENERIC_METHOD]](%swift.opaque* {{.*}}, %swift.opaque* {{.*}}, %swift.type* %U, %swift.type* %T)
  x.generic_method(z)
}
