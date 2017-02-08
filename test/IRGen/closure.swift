// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// -- partial_apply context metadata

// CHECK: [[METADATA:@.*]] = private constant %swift.full_boxmetadata { void (%swift.refcounted*)* @objectdestroy, i8** null, %swift.type { i64 64 }, i32 16, i8* bitcast (<{ i32, i32, i32, i32 }>* @"\01l__swift3_reflection_descriptor" to i8*) }

func a(i i: Int) -> (Int) -> Int {
  return { x in i }
}

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @_T07closure1aSiSicSi1i_tFSiSicfU_(i64, i64)

protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(seq seq: T) -> (Int) -> Int {
  return { i in i + seq.ord() }
}

// -- partial_apply stub
// CHECK: define internal i64 @_T07closure1aSiSicSi1i_tFSiSicfU_TA(i64, %swift.refcounted*)
// CHECK: }

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @_T07closure1bSiSicx3seq_tAA9OrdinableRzlFSiSicfU_(i64, %swift.refcounted*, %swift.type* %T, i8** %T.Ordinable) {{.*}} {

// -- partial_apply stub
// CHECK: define internal i64 @_T07closure1bSiSicx3seq_tAA9OrdinableRzlFSiSicfU_TA(i64, %swift.refcounted*) {{.*}} {
// CHECK: entry:
// CHECK:   [[CONTEXT:%.*]] = bitcast %swift.refcounted* %1 to <{ %swift.refcounted, [16 x i8], %swift.refcounted* }>*
// CHECK:   [[BINDINGSADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, [16 x i8], %swift.refcounted* }>, <{ %swift.refcounted, [16 x i8], %swift.refcounted* }>* [[CONTEXT]], i32 0, i32 1
// CHECK:   [[TYPEADDR:%.*]] = bitcast [16 x i8]* [[BINDINGSADDR]]
// CHECK:   [[TYPE:%.*]] = load %swift.type*, %swift.type** [[TYPEADDR]], align 8
// CHECK:   [[WITNESSADDR_0:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[TYPEADDR]], i32 1
// CHECK:   [[WITNESSADDR:%.*]] = bitcast %swift.type** [[WITNESSADDR_0]]
// CHECK:   [[WITNESS:%.*]] = load i8**, i8*** [[WITNESSADDR]], align 8
// CHECK:   [[BOXADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, [16 x i8], %swift.refcounted* }>, <{ %swift.refcounted, [16 x i8], %swift.refcounted* }>* [[CONTEXT]], i32 0, i32 2
// CHECK:   [[BOX:%.*]] = load %swift.refcounted*, %swift.refcounted** [[BOXADDR]], align 8
// CHECK:   call void @swift_rt_swift_retain(%swift.refcounted* [[BOX]])
// CHECK:   call void @swift_rt_swift_release(%swift.refcounted* %1)
// CHECK:   [[RES:%.*]] = tail call i64 @_T07closure1bSiSicx3seq_tAA9OrdinableRzlFSiSicfU_(i64 %0, %swift.refcounted* [[BOX]], %swift.type* [[TYPE]], i8** [[WITNESS]])
// CHECK:   ret i64 [[RES]]
// CHECK: }

// -- <rdar://problem/14443343> Boxing of tuples with generic elements
// CHECK: define hidden { i8*, %swift.refcounted* } @_T07closure14captures_tuplex_q_tycx_q_t1x_tr0_lF(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* %U)
func captures_tuple<T, U>(x x: (T, U)) -> () -> (T, U) {
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK-NOT: @swift_getTupleTypeMetadata2
  // CHECK: [[BOX:%.*]] = call { %swift.refcounted*, %swift.opaque* } @swift_allocBox(%swift.type* [[METADATA]])
  // CHECK: [[ADDR:%.*]] = extractvalue { %swift.refcounted*, %swift.opaque* } [[BOX]], 1
  // CHECK: bitcast %swift.opaque* [[ADDR]] to <{}>*
  return {x}
}
