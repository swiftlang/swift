// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s

public protocol OS_dispatch_queue {
}
public typealias dispatch_queue_t = OS_dispatch_queue

func dispatch_queue_create() -> dispatch_queue_t! {
  return nil
}

// CHECK: !DIGlobalVariable(name: "queue",
// CHECK-SAME:              line: [[@LINE+6]], type: ![[TY_CONTAINER:[0-9]+]]
// CHECK: ![[TY_CONTAINER]] = !DICompositeType({{.*}}elements: ![[TY_ELTS:[0-9]+]]
// CHECK: ![[TY_ELTS]] = !{![[TY_MEMBER:[0-9]+]]}
// CHECK: ![[TY_MEMBER]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[TY:[0-9]+]]
// CHECK: ![[TY]] = !DICompositeType(
// CHECK-SAME:             identifier: "$s4main16dispatch_queue_taSgD"
public var queue = dispatch_queue_create()
