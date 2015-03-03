// RUN: %target-swift-frontend -emit-ir -g %s -o - | FileCheck %s

// REQUIRES: objc_interop

import Dispatch
// CHECK-DAG: ![[QUEUE_T:[0-9]+]] = !MDCompositeType(tag: DW_TAG_union_type, name: "_TtGSQaSC21dispatch_queue_attr_t_"
// CHECK-DAG: !MDGlobalVariable(name: "queue",{{.*}} line: [[@LINE+1]], type: ![[QUEUE_T]]
var queue = dispatch_queue_create("queue", nil)

dispatch_sync(queue) { println("Hello world"); }
