// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Reparenting -emit-module %S/Inputs/reparentable_lib.swift -o %t
// RUN: %target-swift-frontend -module-name output -enable-experimental-feature Reparenting -emit-ir -parse-as-library %s -I %t -o %t/output.ll
// RUN: %FileCheck --check-prefix=CHECK --check-prefix=DEFAULT %s < %t/output.ll

// RUN: %target-swift-frontend -enable-experimental-feature Reparenting -enable-relative-protocol-witness-tables -emit-module %S/Inputs/reparentable_lib.swift -o %t
// RUN: %target-swift-frontend -module-name output -enable-experimental-feature Reparenting -enable-relative-protocol-witness-tables -emit-ir -parse-as-library %s -I %t -o %t/relative.ll
// RUN: %FileCheck --check-prefix=CHECK --check-prefix=RELATIVE --check-prefix=RELATIVE-%target-cpu %s < %t/relative.ll

// REQUIRES: swift_feature_Reparenting

// REQUIRES: CPU=x86_64 || CPU=arm64 || CPU=arm64e

import reparentable_lib

// CHECK-LABEL: define{{.*}} void @"$s6output9__start__yyxm16reparentable_lib11MultiParentRzlF"(ptr %0, ptr %T, ptr %T.MultiParent)
// CHECK:     [[A_IDX:%.*]] = udiv i64 sub (i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentPAA1ATb" to i64), i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentTL" to i64)), 8
// DEFAULT:   [[A_ADDR:%.*]] = getelementptr inbounds ptr, ptr %T.MultiParent, i64 [[A_IDX]]
// DEFAULT:   %T.A = load ptr, ptr [[A_ADDR]], align 8, !invariant.load
// RELATIVE:  %T.A = call ptr @__swift_relative_protocol_witness_table_parent_dynamic_index_i64(ptr %T.MultiParent, i64 [[A_IDX]])
// CHECK:     call swiftcc void @"$s6output9__upToA__yyxm16reparentable_lib1ARzlF"(ptr %T, ptr %T, ptr %T.A)

// CHECK:     [[B_IDX:%.*]] = udiv i64 sub (i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentPAA1BTb" to i64), i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentTL" to i64)), 8
// DEFAULT:   [[B_ADDR:%.*]] = getelementptr inbounds ptr, ptr %T.MultiParent, i64 [[B_IDX]]
// DEFAULT:   %T.B = load ptr, ptr [[B_ADDR]], align 8, !invariant.load
// RELATIVE:  %T.B = call ptr @__swift_relative_protocol_witness_table_parent_dynamic_index_i64(ptr %T.MultiParent, i64 [[B_IDX]])
// CHECK:     call swiftcc void @"$s6output9__upToB__yyxm16reparentable_lib1BRzlF"(ptr %T, ptr %T, ptr %T.B)

//            ***  NOTE: C is not reparentable ***
// DEFAULT:   [[C_OFFSET:%.*]] = getelementptr inbounds ptr, ptr %T.MultiParent, i32 1
// DEFAULT:   %T.C = load ptr, ptr [[C_OFFSET]], align 8, !invariant.load
// RELATIVE:  %T.C = call ptr @__swift_relative_protocol_witness_table_parent_1(ptr %T.MultiParent)
// CHECK:     call swiftcc void @"$s6output9__upToC__yyxm16reparentable_lib1CRzlF"(ptr %T, ptr %T, ptr %T.C)

// CHECK:     [[D_IDX:%.*]] = udiv i64 sub (i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentPAA1DTb" to i64), i64 ptrtoint (ptr @"$s16reparentable_lib11MultiParentTL" to i64)), 8
// DEFAULT:   [[D_ADDR:%.*]] = getelementptr inbounds ptr, ptr %T.MultiParent, i64 [[D_IDX]]
// DEFAULT:   %T.D = load ptr, ptr [[D_ADDR]], align 8, !invariant.load
// RELATIVE:  %T.D = call ptr @__swift_relative_protocol_witness_table_parent_dynamic_index_i64(ptr %T.MultiParent, i64 [[D_IDX]])
// CHECK:     call swiftcc void @"$s6output9__upToD__yyxm16reparentable_lib1DRzlF"(ptr %T, ptr %T, ptr %T.D)

public func __start__<T: MultiParent>(_: T.Type) {
  __upToA__(T.self)
  __upToB__(T.self)
  __upToC__(T.self)
  __upToD__(T.self)
}

public func __upToA__<T: A>(_: T.Type) {}
public func __upToB__<T: B>(_: T.Type) {}
public func __upToC__<T: C>(_: T.Type) {}
public func __upToD__<T: D>(_: T.Type) {}


// RELATIVE: define{{.*}} hidden ptr @__swift_relative_protocol_witness_table_parent_dynamic_index_i64(ptr [[T_INHERITED:%.*]], i64 [[INDEX:%.*]])
// RELATIVE:  [[T0:%.*]] = ptrtoint ptr [[T_INHERITED]] to i64
// RELATIVE:  [[T1:%.*]] = and i64 [[T0]], 1
// RELATIVE:  [[T2:%.*]] = icmp eq i64 [[T1]], 1
// RELATIVE:  br i1 [[T2]], label %[[L1:.*]], label %[[L2:.*]]

// RELATIVE:[[L1]]:
// RELATIVE:  [[T3:%.*]] = and i64 [[T0]], -2
// RELATIVE:  [[T4:%.*]] = inttoptr i64 [[T3]] to ptr
// RELATIVE:  [[T5:%.*]] = getelementptr inbounds ptr, ptr [[T4]], i64 [[INDEX]]
// RELATIVE:  [[T6:%.*]] = load ptr, ptr [[T5]]
// RELATIVE:  br label %[[L3:.*]]

// RELATIVE:[[L2]]:
// RELATIVE-arm64e:  [[P0:%.*]] = ptrtoint ptr [[T_INHERITED]] to i64
// RELATIVE-arm64e:  [[P1:%.*]] = call i64 @llvm.ptrauth.auth(i64 [[P0]], i32 2, i64 47152)
// RELATIVE-arm64e:  [[T_INHERITED:%.*]] = inttoptr i64 [[P1]] to ptr
// RELATIVE:  [[T9:%.*]] = getelementptr inbounds i32, ptr [[T_INHERITED]], i64 [[INDEX]]
// RELATIVE:  [[T10:%.*]] = load i32, ptr [[T9]]
// RELATIVE:  [[T11:%.*]] = sext i32 [[T10]] to i64
// RELATIVE:  [[T12:%.*]] = ptrtoint ptr [[T9]] to i64
// RELATIVE:  [[T13:%.*]] = add i64 [[T12]], [[T11]]
// RELATIVE:  [[T14:%.*]] = inttoptr i64 [[T13]] to ptr
// RELATIVE-arm64e:  [[T16:%.*]] = ptrtoint ptr [[T14]] to i64
// RELATIVE-arm64e:  [[T17:%.*]] = call i64 @llvm.ptrauth.sign(i64 [[T16]], i32 2, i64 47152)
// RELATIVE-arm64e:  [[T14:%.*]] = inttoptr i64 [[T17]] to ptr
// RELATIVE:  br label %[[L3:.*]]

// RELATIVE:[[L3]]:
// RELATIVE:  phi ptr [ [[T6]], %[[L1]] ], [ [[T14]], %[[L2]] ]
