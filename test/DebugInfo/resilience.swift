
// RUN: %empty-directory(%t)
//
// Compile the external swift module.
// RUN: %target-swift-frontend -g -emit-module -enable-resilience \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
//
// RUN: %target-swift-frontend -g -I %t -emit-ir -enable-resilience %s  -o - \
// RUN:  | %FileCheck %s
//
// RUN: %target-swift-frontend -g -I %t -emit-ir -enable-resilience %s -o - \
// RUN:    -enable-resilience-bypass | %FileCheck %s --check-prefix=CHECK-LLDB
import resilient_struct

let fixed = Point(x: 1, y: 2)
let non_fixed = Size(w: 1, h: 2)
let int = ResilientInt(i: 1)
// CHECK: @"$S10resilience5fixed16resilient_struct5PointVvp" =
// CHECK-SAME: !dbg ![[FIXED:[0-9]+]]
// CHECK: @"$S10resilience9non_fixed16resilient_struct4SizeVvp" =
// CHECK-SAME: !dbg ![[NON_FIXED:[0-9]+]]
// CHECK: @"$S10resilience3int16resilient_struct12ResilientIntVvp" =
// CHECK-SAME: !dbg ![[INT:[0-9]+]]
// CHECK-LABEL: define{{.*}}main

// CHECK-LABEL: define{{.*}} swiftcc void @"$S10resilience9takesSizeyy16resilient_struct0C0VF"(%swift.opaque* noalias nocapture)
// CHECK-LLDB-LABEL: define{{.*}} swiftcc void @"$S10resilience9takesSizeyy16resilient_struct0C0VF"(%T16resilient_struct4SizeV* noalias nocapture dereferenceable({{8|16}}))
public func takesSize(_ s: Size) {}


// CHECK-LABEL: define{{.*}} swiftcc void @"$S10resilience1fyyF"()
// CHECK-LLDB-LABEL: define{{.*}} swiftcc void @"$S10resilience1fyyF"()
func f() {
  let s1 = Size(w: 1, h: 2)
  takesSize(s1)
  // CHECK: %[[ADDR:.*]] = alloca i8*
  // CHECK: call void @llvm.dbg.declare(metadata i8** %[[ADDR]],
  // CHECK-SAME:                        metadata ![[V1:[0-9]+]],
  // CHECK-SAME:                        metadata !DIExpression(DW_OP_deref))
  // CHECK: %[[S1:.*]] = alloca i8,
  // CHECK: store i8* %[[S1]], i8** %[[ADDR]]

  // CHECK-LLDB: %[[ADDR:.*]] = alloca %T16resilient_struct4SizeV
  // CHECK-LLDB: call void @llvm.dbg.declare(metadata %T16resilient_struct4SizeV* %[[ADDR]],
  // CHECK-LLDB-SAME:                        metadata ![[V1:[0-9]+]],
  // CHECK-LLDB-SAME:                        metadata !DIExpression())
}
f()

// Note that these DW_OP_deref are not necessarily correct, but it's the best
// approxmiation we have until LLDB can query the runtime for whether a relient
// type's storage is inline or not.
// CHECK: ![[FIXED]] = !DIGlobalVariableExpression(
// CHECK-SAME:            expr: !DIExpression())
// CHECK: ![[NON_FIXED]] = !DIGlobalVariableExpression(
// CHECK-SAME:            expr: !DIExpression(DW_OP_deref))
// CHECK: ![[TY:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Size",
// CHECK: ![[INT]] = !DIGlobalVariableExpression(
// CHECK-SAME:            expr: !DIExpression(DW_OP_deref))

// CHECK: ![[V1]] = !DILocalVariable(name: "s1", {{.*}}type: ![[TY]])

// CHECK-LLDB: ![[TY:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Size",
// CHECK-LLDB: ![[V1]] = !DILocalVariable(name: "s1", {{.*}}type: ![[TY]])

