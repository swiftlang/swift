// RUN: %target-swift-frontend -emit-ir -g %s -o - | FileCheck %s

// REQUIRES: objc_interop

import Dispatch

func markUsed<T>(t: T) {}

// CHECK-DAG: !DICompositeType(tag: DW_TAG_union_type, {{.*}}identifier: "_TtGSQaSC16dispatch_queue_t_"
// CHECK-DAG: !DIGlobalVariable(name: "queue",{{.*}} line: [[@LINE+1]], type: !"_TtGSQaSC16dispatch_queue_t_"
var queue = dispatch_queue_create("queue", nil)

dispatch_sync(queue) { markUsed("Hello world"); }
