// RUN: %swift -emit-silgen %s | FileCheck %s

var a = 2

// CHECK: assign {{%.*}} to {{%.*}} : $*Int64
// CHECK: destroy_value {{%.*}} : $String
(a, _) = (2, "three")
