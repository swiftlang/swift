// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s

public protocol OS_dispatch_queue {
}
public typealias dispatch_queue_t = OS_dispatch_queue

func dispatch_queue_create() -> dispatch_queue_t! {
  return nil
}

// CHECK: !DIGlobalVariable(name: "queue",
// CHECK-SAME:              line: [[@LINE+3]], type: ![[T:[0-9]+]]
// CHECK: ![[T]] = !DICompositeType(
// CHECK-SAME:             identifier: "_T04main16dispatch_queue_taSgD"
public var queue = dispatch_queue_create()
