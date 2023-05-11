// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir
// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CAPTURE
// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: PTRSIZE=64

// CHECK-DAG: [[FILENAME:@"\.str\..*closure\.swift"]] = {{.*}} c"{{.*}}closure.swift\00"
// OPT: [[FILENAME:@\.str\.0\.]] = {{.*}} [1 x i8] zeroinitializer

// -- partial_apply context metadata

// CHECK-DAG: [[METADATA:@.*]] = private constant %swift.full_boxmetadata { void (%swift.refcounted*)* {{.*}}@objectdestroy{{(\.ptrauth.*)?}}, i8** null, %swift.type { i64 1024 }, i32 16, i8* bitcast ({ i32, i32, i32, i32 }* @"\01l__swift5_reflection_descriptor" to i8*) }

func a(i i: Int) -> (Int) -> Int {
  return { x in i }
}

// -- Closure entry point
// CHECK: define internal swiftcc i64 @"$s7closure1a1iS2icSi_tFS2icfU_"(i64 %0, i64 %1)

protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(seq seq: T) -> (Int) -> Int {
  return { i in i + seq.ord() }
}

// -- partial_apply stub
// CHECK: define internal swiftcc i64 @"$s7closure1a1iS2icSi_tFS2icfU_TA"(i64 %0, %swift.refcounted* swiftself %1)
// CHECK: }

// -- Closure entry point
// CHECK: define internal swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_"(i64 %0, %swift.opaque* noalias nocapture %1, %swift.type* %T, i8** %T.Ordinable) {{.*}} {

// -- partial_apply stub
// CHECK: define internal swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_TA"(i64 %0, %swift.refcounted* swiftself %1) {{.*}} {
// CHECK: entry:
// CHECK:   [[CONTEXT:%.*]] = bitcast %swift.refcounted* %1 to <{ %swift.refcounted, [16 x i8] }>*
// CHECK:   [[BINDINGSADDR:%.*]] = getelementptr inbounds <{ %swift.refcounted, [16 x i8] }>, <{ %swift.refcounted, [16 x i8] }>* [[CONTEXT]], i32 0, i32 1
// CHECK:   [[TYPEADDR:%.*]] = bitcast [16 x i8]* [[BINDINGSADDR]]
// CHECK:   [[TYPE:%.*]] = load %swift.type*, %swift.type** [[TYPEADDR]], align 8
// CHECK:   [[WITNESSADDR_0:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[TYPEADDR]], i32 1
// CHECK:   [[WITNESSADDR:%.*]] = bitcast %swift.type** [[WITNESSADDR_0]]
// CHECK:   [[WITNESS:%.*]] = load i8**, i8*** [[WITNESSADDR]], align 8
// CHECK:   [[RES:%.*]] = tail call swiftcc i64 @"$s7closure1b3seqS2icx_tAA9OrdinableRzlFS2icfU_"(i64 %0, %swift.opaque* noalias nocapture {{.*}}, %swift.type* [[TYPE]], i8** [[WITNESS]])
// CHECK:   ret i64 [[RES]]
// CHECK: }

// -- <rdar://problem/14443343> Boxing of tuples with generic elements
// CHECK: define hidden swiftcc { i8*, %swift.refcounted* } @"$s7closure14captures_tuple1xx_q_tycx_q_t_tr0_lF"(%swift.opaque* noalias nocapture %0, %swift.opaque* noalias nocapture %1, %swift.type* %T, %swift.type* %U)
func captures_tuple<T, U>(x x: (T, U)) -> () -> (T, U) {
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @swift_getTupleTypeMetadata2(i64 0, %swift.type* %T, %swift.type* %U, i8* null, i8** null)
  // CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK-NOT: @swift_getTupleTypeMetadata2
  // CHECK: ret
  return {x}
}

class C {}

func useClosure(_ cl : () -> ()) {}

// CAPTURE-NOT: reflection_descriptor{{.*}} = private constant { i32, i32, i32, i32, i32, i32, i32, i32 } { i32 5, i32 0, i32 0
func no_capture_descriptor(_ c: C, _ d: C, _ e: C, _ f: C, _ g: C) {
  useClosure( { _ = c ; _ = d ; _ = e ; _ = f ; _ = g })
}

// CHECK-LABEL: define hidden swiftcc { i8*, %swift.refcounted* } @"$s7closure9letEscape1fyycyyXE_tF"(i8* %0, %swift.opaque* %1)
// CHECK: call zeroext i1 @swift_isEscapingClosureAtFileLocation(%swift.refcounted* {{.*}}, i8* getelementptr inbounds ({{.*}} [[FILENAME]]
// OPT-LABEL: define hidden swiftcc { i8*, %swift.refcounted* } @"$s7closure9letEscape1fyycyyXE_tF"(i8* %0, %swift.opaque* %1)
// OPT: call zeroext i1 @swift_isEscapingClosureAtFileLocation(%swift.refcounted* {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ({{.*}} [[FILENAME]]
func letEscape(f: () -> ()) -> () -> () {
  return withoutActuallyEscaping(f) { return $0 }
}
