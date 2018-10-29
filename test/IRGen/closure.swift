// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// -- partial_apply context metadata

// CHECK: [[METADATA:@.*]] = private constant %swift.full_boxmetadata { void (%swift.refcounted*)* @objectdestroy, i8** null, %swift.type { i64 1024 }, i32 16, i8* bitcast ({ i32, i32, i32, i32 }* @"\01l__swift5_reflection_descriptor" to i8*) }

func a(i i: Int) -> (Int) -> Int {
  return { x in i }
}

// -- Closure entry point
// CHECK: define internal swiftcc i64 @"$s7closure1a1iS2icSi_tFS2icfU_"(i64, i64)

protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(seq seq: T) -> (Int) -> Int {
  return { i in i + seq.ord() }
}

// -- partial_apply stub
// CHECK: define internal swiftcc i64 @"$s7closure1a1iS2icSi_tFS2icfU_TA"(i64, %swift.refcounted* swiftself)
// CHECK: }

// -- Closure entry point
// CHECK: define internal swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_"(i64, %swift.refcounted*, %swift.type* %T, i8** %T.Ordinable) {{.*}} {

// -- partial_apply stub
// CHECK: define internal swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_TA"(i64, %swift.refcounted* swiftself) {{.*}} {
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
// CHECK:   [[RES:%.*]] = tail call swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_"(i64 %0, %swift.refcounted* [[BOX]], %swift.type* [[TYPE]], i8** [[WITNESS]])
// CHECK:   ret i64 [[RES]]
// CHECK: }

// -- <rdar://problem/14443343> Boxing of tuples with generic elements
// CHECK: define hidden swiftcc { i8*, %swift.refcounted* } @"$s7closure14captures_tuple1xx_q_tycx_q_t_tr0_lF"(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* %U)
func captures_tuple<T, U>(x x: (T, U)) -> () -> (T, U) {
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata2(i64 0, %swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK-NOT: @swift_getTupleTypeMetadata2
  // CHECK: [[BOX:%.*]] = call swiftcc { %swift.refcounted*, %swift.opaque* } @swift_allocBox(%swift.type* [[METADATA]])
  // CHECK: [[ADDR:%.*]] = extractvalue { %swift.refcounted*, %swift.opaque* } [[BOX]], 1
  // CHECK: bitcast %swift.opaque* [[ADDR]] to <{}>*
  return {x}
}
