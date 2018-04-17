
// RUN: %target-swift-frontend -module-name generic_tuples -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

// Make sure that optimization passes don't choke on storage types for generic tuples
// RUN: %target-swift-frontend -module-name generic_tuples -assume-parsing-unqualified-ownership-sil -emit-ir -O %s

// REQUIRES: CPU=x86_64

// CHECK: [[TYPE:%swift.type]] = type {
// CHECK: [[OPAQUE:%swift.opaque]] = type opaque
// CHECK: [[TUPLE_TYPE:%swift.tuple_type]] = type { [[TYPE]], i64, i8*, [0 x %swift.tuple_element_type] }
// CHECK: %swift.tuple_element_type = type { [[TYPE]]*, i64 }

func dup<T>(_ x: T) -> (T, T) { var x = x; return (x,x) }
// CHECK:    define hidden swiftcc void @"$S14generic_tuples3dupyx_xtxlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
// CHECK:    entry:
//   Allocate a local variable for 'x'.
// CHECK: [[TYPE_ADDR:%.*]] = bitcast %swift.type* %T to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[TYPE_ADDR]], i64 -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// CHECK: [[SIZE_WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 9
// CHECK: [[SIZE_WITNESS:%.*]] = load i8*, i8** [[SIZE_WITNESS_ADDR]]
// CHECK: [[SIZE:%.*]] = ptrtoint i8* [[SIZE_WITNESS]]
// CHECK: [[X_ALLOCA:%.*]] = alloca i8, {{.*}} [[SIZE]], align 16
// CHECK: [[X_TMP:%.*]] = bitcast i8* [[X_ALLOCA]] to %swift.opaque*
// Debug info shadow copy.
// CHECK-NEXT: store i8* [[X_ALLOCA]]
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]], align 8
// CHECK-NEXT: [[INITIALIZE_WITH_COPY:%.*]] = bitcast i8* [[WITNESS]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: [[X:%.*]] = call [[OPAQUE]]* [[INITIALIZE_WITH_COPY]]([[OPAQUE]]* noalias [[X_TMP]], [[OPAQUE]]* noalias {{.*}}, [[TYPE]]* %T)
//   Copy 'x' into the first result.
// CHECK-NEXT: call [[OPAQUE]]* [[INITIALIZE_WITH_COPY]]([[OPAQUE]]* noalias %0, [[OPAQUE]]* noalias [[X_TMP]], [[TYPE]]* %T)
//   Copy 'x' into the second element.
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]], align 8
// CHECK-NEXT: [[TAKE_FN:%.*]] = bitcast i8* [[WITNESS]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: call [[OPAQUE]]* [[TAKE_FN]]([[OPAQUE]]* noalias %1, [[OPAQUE]]* noalias [[X_TMP]], [[TYPE]]* %T)

struct S {}


func callDup(_ s: S) { _ = dup(s) }
// CHECK-LABEL: define hidden swiftcc void @"$S14generic_tuples7callDupyyAA1SVF"()
// CHECK-NEXT: entry:
// CHECK-NEXT: call swiftcc void @"$S14generic_tuples3dupyx_xtxlF"({{.*}} undef, {{.*}} undef, %swift.type* {{.*}} @"$S14generic_tuples1SVMf", {{.*}})
// CHECK-NEXT: ret void

class C {}

func dupC<T : C>(_ x: T) -> (T, T) { return (x, x) }
// CHECK-LABEL: define hidden swiftcc { %T14generic_tuples1CC*, %T14generic_tuples1CC* } @"$S14generic_tuples4dupCyx_xtxAA1CCRbzlF"(%T14generic_tuples1CC*, %swift.type* %T)
// CHECK-NEXT: entry:
// CHECK:      [[REF:%.*]] = bitcast %T14generic_tuples1CC* %0 to %swift.refcounted*
// CHECK-NEXT: call %swift.refcounted* @swift_retain(%swift.refcounted* returned [[REF]])
// CHECK-NEXT: [[REF:%.*]] = bitcast %T14generic_tuples1CC* %0 to %swift.refcounted*
// CHECK-NEXT: call %swift.refcounted* @swift_retain(%swift.refcounted* returned [[REF]])
// CHECK-NEXT: [[TUP1:%.*]] = insertvalue { %T14generic_tuples1CC*, %T14generic_tuples1CC* } undef, %T14generic_tuples1CC* %0, 0
// CHECK-NEXT: [[TUP2:%.*]] = insertvalue { %T14generic_tuples1CC*, %T14generic_tuples1CC* } [[TUP1:%.*]], %T14generic_tuples1CC* %0, 1
// CHECK-NEXT: ret { %T14generic_tuples1CC*, %T14generic_tuples1CC* } [[TUP2]]

func callDupC(_ c: C) { _ = dupC(c) }
// CHECK-LABEL: define hidden swiftcc void @"$S14generic_tuples8callDupCyyAA1CCF"(%T14generic_tuples1CC*)
// CHECK-NEXT: entry:
// CHECK-NEXT: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14generic_tuples1CCMa"(i64 0)
// CHECK-NEXT: [[METATYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[TUPLE:%.*]] = call swiftcc { %T14generic_tuples1CC*, %T14generic_tuples1CC* } @"$S14generic_tuples4dupCyx_xtxAA1CCRbzlF"(%T14generic_tuples1CC* %0, %swift.type* [[METATYPE]])
// CHECK-NEXT: [[LEFT:%.*]] = extractvalue { %T14generic_tuples1CC*, %T14generic_tuples1CC* } [[TUPLE]], 0
// CHECK-NEXT: [[RIGHT:%.*]] = extractvalue { %T14generic_tuples1CC*, %T14generic_tuples1CC* } [[TUPLE]], 1
// CHECK-NEXT: [[LEFT_CAST:%.*]] = bitcast %T14generic_tuples1CC* [[LEFT]] to %swift.refcounted*
// CHECK-NEXT: call %swift.refcounted* @swift_retain(%swift.refcounted* returned [[LEFT_CAST]]
// CHECK-NEXT: [[RIGHT_CAST:%.*]] = bitcast %T14generic_tuples1CC* [[RIGHT]] to %swift.refcounted*
// CHECK-NEXT: call %swift.refcounted* @swift_retain(%swift.refcounted* returned [[RIGHT_CAST]]
// CHECK-NEXT: call void bitcast (void (%swift.refcounted*)* @swift_release to void (%T14generic_tuples1CC*)*)(%T14generic_tuples1CC* [[LEFT]])
// CHECK-NEXT: call void bitcast (void (%swift.refcounted*)* @swift_release to void (%T14generic_tuples1CC*)*)(%T14generic_tuples1CC* [[RIGHT]])
// CHECK-NEXT: call void bitcast (void (%swift.refcounted*)* @swift_release to void (%T14generic_tuples1CC*)*)(%T14generic_tuples1CC* [[RIGHT]])
// CHECK-NEXT: call void bitcast (void (%swift.refcounted*)* @swift_release to void (%T14generic_tuples1CC*)*)(%T14generic_tuples1CC* [[LEFT]])
// CHECK-NEXT: ret void

// CHECK: define hidden swiftcc i64 @"$S14generic_tuples4lumpySi_xxtxlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump<T>(_ x: T) -> (Int, T, T) { return (0,x,x) }
// CHECK: define hidden swiftcc { i64, i64 } @"$S14generic_tuples5lump2ySi_SixtxlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump2<T>(_ x: T) -> (Int, Int, T) { return (0,0,x) }
// CHECK: define hidden swiftcc void @"$S14generic_tuples5lump3yx_xxtxlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump3<T>(_ x: T) -> (T, T, T) { return (x,x,x) }
// CHECK: define hidden swiftcc i64 @"$S14generic_tuples5lump4yx_SixtxlF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump4<T>(_ x: T) -> (T, Int, T) { return (x,0,x) }

// CHECK: define hidden swiftcc i64 @"$S14generic_tuples6unlumpyS2i_xxt_tlF"(i64, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump<T>(_ x: (Int, T, T)) -> Int { return x.0 }
// CHECK: define hidden swiftcc void @"$S14generic_tuples7unlump1yxSi_xxt_tlF"(%swift.opaque* noalias nocapture sret, i64, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump1<T>(_ x: (Int, T, T)) -> T { return x.1 }
// CHECK: define hidden swiftcc void @"$S14generic_tuples7unlump2yxx_Sixt_tlF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump2<T>(_ x: (T, Int, T)) -> T { return x.0 }
// CHECK: define hidden swiftcc i64 @"$S14generic_tuples7unlump3ySix_Sixt_tlF"(%swift.opaque* noalias nocapture, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump3<T>(_ x: (T, Int, T)) -> Int { return x.1 }


// CHECK: tuple_existentials
func tuple_existentials() {
  // Empty tuple:
  var a : Any = ()
  // CHECK: store %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$SytN", i32 0, i32 1),

  // 2 element tuple
  var t2 = (1,2.0)
  a = t2
  // CHECK: call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata2(i64 %0, {{.*}}@"$SSiN{{.*}}",{{.*}}@"$SSdN{{.*}}", i8* null, i8** null)


  // 3 element tuple
  var t3 = ((),(),())
  a = t3
  // CHECK: call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata3(i64 %0, {{.*}}@"$SytN{{.*}},{{.*}}@"$SytN{{.*}},{{.*}}@"$SytN{{.*}}, i8* null, i8** null)

  // 4 element tuple
  var t4 = (1,2,3,4)
  a = t4
  // CHECK: call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata(i64 %0, i64 4, {{.*}}, i8* null, i8** null)
}

