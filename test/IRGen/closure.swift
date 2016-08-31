// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// -- partial_apply context metadata

// CHECK: [[METADATA:@.*]] = private constant %swift.full_boxmetadata { void (%swift.refcounted*)* @objectdestroy, i8** null, %swift.type { i64 64 }, i32 16, i8* bitcast (<{ i32, i32, i32, i32 }>* @"\01l__swift3_capture_descriptor" to i8*) }

func a(i i: Int) -> (Int) -> Int {
  return { x in i }
}

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @_TFF7closure1aFT1iSi_FSiSiU_FSiSi(i64, i64)

protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(seq seq: T) -> (Int) -> Int {
  return { i in i + seq.ord() }
}

// -- partial_apply stub
// CHECK: define internal i64 @_TPA__TFF7closure1aFT1iSi_FSiSiU_FSiSi(i64, %swift.refcounted*)
// CHECK: }

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @[[CLOSURE2:_TFF7closure1buRxS_9OrdinablerFT3seqx_FSiSiU_FSiSi]](i64, %swift.refcounted*, %swift.type* %T, i8** %T.Ordinable) {{.*}} {

// -- partial_apply stub
// CHECK: define internal i64 @_TPA_[[CLOSURE2]](i64, %swift.refcounted*) {{.*}} {
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
// CHECK:   call void @rt_swift_retain(%swift.refcounted* [[BOX]])
// CHECK:   call void @rt_swift_release(%swift.refcounted* %1)
// CHECK:   [[RES:%.*]] = tail call i64 @[[CLOSURE2]](i64 %0, %swift.refcounted* [[BOX]], %swift.type* [[TYPE]], i8** [[WITNESS]])
// CHECK:   ret i64 [[RES]]
// CHECK: }

// -- <rdar://problem/14443343> Boxing of tuples with generic elements
// CHECK: define hidden { i8*, %swift.refcounted* } @_TF7closure14captures_tupleu0_rFT1xTxq___FT_Txq__(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* %U)
func captures_tuple<T, U>(x x: (T, U)) -> () -> (T, U) {
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK-NOT: @swift_getTupleTypeMetadata2
  // CHECK: [[BOX:%.*]] = call { %swift.refcounted*, %swift.opaque* } @swift_allocBox(%swift.type* [[METADATA]])
  // CHECK: [[ADDR:%.*]] = extractvalue { %swift.refcounted*, %swift.opaque* } [[BOX]], 1
  // CHECK: bitcast %swift.opaque* [[ADDR]] to <{}>*
  return {x}
}
