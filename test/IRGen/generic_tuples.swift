// RUN: %swift -triple x86_64-apple-darwin10 -emit-llvm %s | FileCheck %s

// CHECK: [[OPAQUE:%swift.opaque]] = type opaque
// CHECK: [[TYPE:%swift.type]] = type {
// CHECK: [[TUPLE_TYPE:%swift.tuple_type]] = type { [[TYPE]], i64, i8*, [0 x %swift.tuple_element_type] }
// CHECK: %swift.tuple_element_type = type { [[TYPE]]*, i64 }

func dup<T>(x : T) -> (T, T) { return (x,x) }
// CHECK:    define void @_T14generic_tuples3dupU__FT1xQ__TQ_Q__({ [[OPAQUE]] }* noalias sret
// CHECK:      [[ELTS:%.*]] = alloca [2 x [[TYPE]]*], align 8
//   Get value witnesses for T.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* %T to i8***
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8*** [[T0]], i64 -1
// CHECK-NEXT: %T.value = load i8*** [[T1]], align 8
//   Project to first element of tuple.
// CHECK-NEXT: [[FST:%.*]] = bitcast { [[OPAQUE]] }* [[RET:%.*]] to [[OPAQUE]]*
//   Get the tuple metadata.
//   FIXME: maybe this should get passed in?
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [2 x [[TYPE]]*]* [[ELTS]], i32 0, i32 0
// CHECK-NEXT: store [[TYPE]]* %T, [[TYPE]]** [[T0]], align 8
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [2 x [[TYPE]]*]* [[ELTS]], i32 0, i32 1
// CHECK-NEXT: store [[TYPE]]* %T, [[TYPE]]** [[T1]], align 8
// CHECK-NEXT: [[TT_PAIR:%.*]] = call [[TYPE]]* @swift_getTupleTypeMetadata(i64 2, [[TYPE]]** [[T0]], i8* null, i8** null)
//   Pull out the offset of the second element and GEP to that.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TYPE]]* [[TT_PAIR]] to [[TUPLE_TYPE]]*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[TUPLE_TYPE]]* [[T0]], i64 0, i32 3, i64 1, i32 1
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T3:%.*]] = bitcast { [[OPAQUE]] }* [[RET]] to i8*
// CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds i8* [[T3]], i64 [[T2]]
// CHECK-NEXT: [[SND:%.*]] = bitcast i8* [[T4]] to [[OPAQUE]]*
//   Copy 'x' into the first element.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[COPY_FN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: call [[OPAQUE]]* [[COPY_FN]]([[OPAQUE]]* [[FST]], [[OPAQUE]]* [[X:%.*]], [[TYPE]]* %T)
//   Copy 'x' into the second element.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 6
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[COPY_FN:%.*]] = bitcast i8* [[T1]] to [[OPAQUE]]* ([[OPAQUE]]*, [[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: call [[OPAQUE]]* [[COPY_FN]]([[OPAQUE]]* [[SND]], [[OPAQUE]]* [[X]], [[TYPE]]* %T)
//   Destroy 'x'.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** %T.value, i32 4
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[DESTROY_FN:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*, [[TYPE]]*)*
// CHECK-NEXT: call void [[DESTROY_FN]]([[OPAQUE]]* [[X]], [[TYPE]]* %T)
// CHECK-NEXT: ret void
