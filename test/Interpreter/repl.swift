// RUN: %swift -I %S/.. < %s -repl | FileCheck %s

false
// CHECK: false
(1,2)
// CHECK: (1, 2)
println(10)
// CHECK: 10
// CHECK: ()
