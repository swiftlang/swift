// RUN: %target-swift-frontend -emit-ir -g %s -o - | FileCheck %s

// REQUIRES: objc_interop

import Dispatch
// CHECK-DAG: !DICompositeType(tag: DW_TAG_union_type, {{.*}}identifier: "_TtGSQaSC21dispatch_queue_attr_t_"
// CHECK-DAG: !DIGlobalVariable(name: "queue",{{.*}} line: [[@LINE+1]], type: !"_TtGSQaSC21dispatch_queue_attr_t_"
var queue = dispatch_queue_create("queue", nil)

dispatch_sync(queue) { println("Hello world"); }
