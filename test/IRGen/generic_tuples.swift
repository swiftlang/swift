// RUN: %target-swift-frontend -emit-ir -primary-file %s | FileCheck %s

// Make sure that optimization passes don't choke on storage types for generic tuples
// RUN: %target-swift-frontend -emit-ir -O %s

// REQUIRES: CPU=x86_64

// CHECK: [[TYPE:%swift.type]] = type {
// CHECK: [[OPAQUE:%swift.opaque]] = type opaque
// CHECK: [[TUPLE_TYPE:%swift.tuple_type]] = type { [[TYPE]], i64, i8*, [0 x %swift.tuple_element_type] }
// CHECK: %swift.tuple_element_type = type { [[TYPE]]*, i64 }

func dup<T>(var x: T) -> (T, T) { return (x,x) }
// CHECK:    define hidden void @_TF14generic_tuples3dup{{.*}}(<{}>* noalias nocapture sret
// CHECK:    entry:
//   Allocate a local variable for 'x'.
// CHECK-NEXT: alloca %swift.type*, align 8
// CHECK-NEXT: [[XBUF:%.*]] = alloca [[BUFFER:.*]], align 8
// CHECK-NEXT: store %swift.type*
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], i64 -1
// CHECK-NEXT: [[T_VALUE:%.*]] = load i8**, i8*** [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T_VALUE]], i32 11
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]], align 8
// CHECK-NEXT: [[ALLOCATE_FN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[BUFFER]]*, [[TYPE]]*)*
// CHECK-NEXT: [[X:%.*]] = call [[OPAQUE]]* [[ALLOCATE_FN]]([[BUFFER]]* [[XBUF]], [[TYPE]]* %T)
//   Get value witnesses for T.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T_VALUE]], i32 9
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]], align 8
// CHECK-NEXT: [[TAKE_FN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
//   Project to first element of tuple.
// CHECK: [[FST:%.*]] = bitcast <{}>* [[RET:%.*]] to [[OPAQUE]]*
//   Get the tuple metadata.
//   FIXME: maybe this should get passed in?
// CHECK-NEXT: [[TT_PAIR:%.*]] = call [[TYPE]]* @swift_getTupleTypeMetadata2([[TYPE]]* %T, [[TYPE]]* %T, i8* null, i8** null)
//   Pull out the offset of the second element and GEP to that.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* [[TT_PAIR]] to [[TUPLE_TYPE]]*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[TUPLE_TYPE]], [[TUPLE_TYPE]]* [[T0]], i64 0, i32 3, i64 1, i32 1
// CHECK-NEXT: [[T2:%.*]] = load i64, i64* [[T1]], align 8
// CHECK-NEXT: [[T3:%.*]] = bitcast <{}>* [[RET]] to i8*
// CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds i8, i8* [[T3]], i64 [[T2]]
// CHECK-NEXT: [[SND:%.*]] = bitcast i8* [[T4]] to [[OPAQUE]]*
//   Copy 'x' into the first element.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T_VALUE]], i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]], align 8
// CHECK-NEXT: [[COPY_FN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: call [[OPAQUE]]* [[COPY_FN]]([[OPAQUE]]* [[FST]], [[OPAQUE]]* [[X]], [[TYPE]]* %T)
//   Copy 'x' into the second element.
// CHECK-NEXT: call [[OPAQUE]]* [[TAKE_FN]]([[OPAQUE]]* [[SND]], [[OPAQUE]]* [[X]], [[TYPE]]* %T)

// CHECK: ret void

// CHECK: define hidden void @_TF14generic_tuples4lump{{.*}}(<{ %Si }>* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump<T>(x: T) -> (Int, T, T) { return (0,x,x) }
// CHECK: define hidden void @_TF14generic_tuples5lump2{{.*}}(<{ %Si, %Si }>* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump2<T>(x: T) -> (Int, Int, T) { return (0,0,x) }
// CHECK: define hidden void @_TF14generic_tuples5lump3{{.*}}(<{}>* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump3<T>(x: T) -> (T, T, T) { return (x,x,x) }
// CHECK: define hidden void @_TF14generic_tuples5lump4{{.*}}(<{}>* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T)
func lump4<T>(x: T) -> (T, Int, T) { return (x,0,x) }

// CHECK: define hidden i64 @_TF14generic_tuples6unlump{{.*}}(i64, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump<T>(x: (Int, T, T)) -> Int { return x.0 }
// CHECK: define hidden void @_TF14generic_tuples7unlump{{.*}}(%swift.opaque* noalias nocapture sret, i64, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump1<T>(x: (Int, T, T)) -> T { return x.1 }
// CHECK: define hidden void @_TF14generic_tuples7unlump2{{.*}}(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump2<T>(x: (T, Int, T)) -> T { return x.0 }
// CHECK: define hidden i64 @_TF14generic_tuples7unlump3{{.*}}(%swift.opaque* noalias nocapture, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
func unlump3<T>(x: (T, Int, T)) -> Int { return x.1 }


// CHECK: tuple_existentials
func tuple_existentials() {
  // Empty tuple:
  var a : Any = ()
  // CHECK: store %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMdT_, i32 0, i32 1),

  // 2 element tuple
  var t2 = (1,2.0)
  a = t2
  // CHECK: call %swift.type* @swift_getTupleTypeMetadata2({{.*}}@_TMdSi{{.*}},{{.*}}@_TMdSd{{.*}}, i8* null, i8** null)


  // 3 element tuple
  var t3 = ((),(),())
  a = t3
  // CHECK: call %swift.type* @swift_getTupleTypeMetadata3({{.*}}@_TMdT_{{.*}},{{.*}}@_TMdT_{{.*}},{{.*}}@_TMdT_{{.*}}, i8* null, i8** null)

  // 4 element tuple
  var t4 = (1,2,3,4)
  a = t4
  // CHECK: call %swift.type* @swift_getTupleTypeMetadata(i64 4, {{.*}}, i8* null, i8** null)
}

