// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -g -emit-module -enable-resilience \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -g -I %t -emit-ir -enable-resilience %s \
// RUN:   | %FileCheck %s
import resilient_struct

func use<T>(_ t: T) {}

public func f() {
  let s1 = Size(w: 1, h: 2)
  use(s1)
  // CHECK: %[[ADDR:.*]] = alloca i8*
  // CHECK: call void @llvm.dbg.declare(metadata i8** %[[ADDR]],
  // CHECK-SAME:                        metadata ![[V1:[0-9]+]],
  // CHECK-SAME:                        metadata !DIExpression(DW_OP_deref))
  // CHECK: %[[USE_BUFFER:.*]] = alloca i8,
  // CHECK: %[[S1:.*]] = alloca i8,
  // CHECK: store i8* %[[S1]], i8** %[[ADDR]]
  // CHECK: ![[V1]] = !DILocalVariable(name: "s1", {{.*}}type: ![[TY:[0-9]+]])
  // CHECK: ![[TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Size",
  // FIXME-NOT: size:
  // CHECK: =
}
