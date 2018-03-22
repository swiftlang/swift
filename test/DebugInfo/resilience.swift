
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
// RUN: %target-swift-frontend -g -I %t -emit-sil -enable-resilience %s -o - \
// RUN:    | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-swift-frontend -g -I %t -emit-sil -enable-resilience %s -o - \
// RUN:    -debugger-support | %FileCheck %s --check-prefix=CHECK-LLDB
import resilient_struct

func use<T>(_ t: T) {}

public func f() {
  let s1 = Size(w: 1, h: 2)
  use(s1)
  // CHECK: %[[ADDR:.*]] = alloca i8*
  // CHECK: call void @llvm.dbg.declare(metadata i8** %[[ADDR]],
  // CHECK-SAME:                        metadata ![[V1:[0-9]+]],
  // CHECK-SAME:                        metadata !DIExpression(DW_OP_deref))
  // CHECK: %[[S1:.*]] = alloca i8,
  // CHECK: store i8* %[[S1]], i8** %[[ADDR]]
  // CHECK: ![[V1]] = !DILocalVariable(name: "s1", {{.*}}type: ![[TY:[0-9]+]])
  // CHECK: ![[TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Size",
  // FIXME-NOT: size:
  // CHECK: =
}

// CHECK-SIL: // f()
// CHECK-LLDB: // f()
// CHECK-SIL: %0 = alloc_stack $Size, let, name "s1"
// CHECK-LLDB: %0 = metatype $@thin Size.Type
// CHECK-LLDB: debug_value %{{.*}} : $Size, let, name "s1"
