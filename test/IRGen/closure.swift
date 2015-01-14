// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir | FileCheck %s

// -- partial_apply context metadata

// CHECK: [[METADATA:@.*]] = private constant %swift.full_heapmetadata { void (%swift.refcounted*)* [[DESTROY:@objectdestroy1]], i8** null, %swift.type { i64 64 } }

func a(var #i: Int) -> (Int) -> Int {
  return {x in i }
}

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @[[CLOSURE:_TTSf2n_d_i___TFF7closure1aFT1iSi_FSiSiU_FSiSi]](i64, i64)

// -- partial_apply stub
// CHECK: define internal i64 @_TPA_[[CLOSURE]](i64, %swift.refcounted*) {
// CHECK: }

protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(var #seq: T) -> (Int) -> Int {
  return {i in i + seq.ord() }
}

// -- Closure entry point
// CHECK: define linkonce_odr hidden i64 @[[CLOSURE:_TFF7closure1bUS_9Ordinable__FT3seqQ__FSiSiU_FSiSi]](i64, %swift.refcounted*, %swift.opaque* noalias, %swift.type* %T, i8** %T.Ordinable) {
// -- partial_apply stub
// CHECK: define internal i64 @_TPA_[[CLOSURE]](i64, %swift.refcounted*) {
// CHECK: entry:
// CHECK:   [[CONTEXT:%.*]] = bitcast %swift.refcounted* %1 to <{ %swift.refcounted, %swift.refcounted*, %swift.opaque*, %swift.type*, i8** }>*
// CHECK:   [[BOXADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, %swift.refcounted*, %swift.opaque*, %swift.type*, i8** }>* [[CONTEXT]], i32 0, i32 1
// CHECK:   [[BOX:%.*]] = load %swift.refcounted** [[BOXADDR]], align 8
// CHECK:   call void @swift_retain_noresult(%swift.refcounted* [[BOX]])
// CHECK:   [[ADDRADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, %swift.refcounted*, %swift.opaque*, %swift.type*, i8** }>* [[CONTEXT]], i32 0, i32 2
// CHECK:   [[ADDR:%.*]] = load %swift.opaque** [[ADDRADDR]], align 8
// CHECK:   [[TYPEADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, %swift.refcounted*, %swift.opaque*, %swift.type*, i8** }>* [[CONTEXT]], i32 0, i32 3
// CHECK:   [[TYPE:%.*]] = load %swift.type** [[TYPEADDR]], align 8
// CHECK:   [[WITNESSADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, %swift.refcounted*, %swift.opaque*, %swift.type*, i8** }>* [[CONTEXT]], i32 0, i32 4
// CHECK:   [[WITNESS:%.*]] = load i8*** [[WITNESSADDR]], align 8
// CHECK:   call void @swift_release(%swift.refcounted* %1)
// CHECK:   [[RES:%.*]] = tail call i64 @[[CLOSURE]](i64 %0, %swift.refcounted* [[BOX]], %swift.opaque* noalias [[ADDR]], %swift.type* [[TYPE]], i8** [[WITNESS]])
// CHECK:   ret i64 %11
// CHECK: }

// -- <rdar://problem/14443343> Boxing of tuples with generic elements
// CHECK: define hidden { i8*, %swift.refcounted* } @_TF7closure14captures_tupleU___FT1xTQ_Q0___FT_TQ_Q0__(%swift.opaque* noalias, %swift.opaque* noalias, %swift.type* %T, %swift.type* %U)
func captures_tuple<T, U>(var #x: (T, U)) -> () -> (T, U) {
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getTupleTypeMetadata2(%swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK: [[BOX:%.*]] = call { %swift.refcounted*, %swift.opaque* } @swift_allocBox(%swift.type* [[METADATA]])
  // CHECK: [[ADDR:%.*]] = extractvalue { %swift.refcounted*, %swift.opaque* } [[BOX]], 1
  // CHECK: bitcast %swift.opaque* [[ADDR]] to <{}>*
  return {x}
}
