// RUN: rm -rf %t/clang-module-cache
// RUN: %target-build-swift -module-cache-path %t/clang-module-cache -emit-ir -g %s -o - | FileCheck %s
// XFAIL: linux

import Dispatch
// CHECK-DAG: ![[QUEUE_T:[0-9]+]] = {{.*}} [_TtGSQaSC21dispatch_queue_attr_t_]
// CHECK-DAG: metadata ![[QUEUE_T]]{{.*}} ; [ DW_TAG_variable ] [queue] [line [[@LINE+1]]] [def]
var queue = dispatch_queue_create("queue", nil)

dispatch_sync(queue) { println("Hello world"); }
