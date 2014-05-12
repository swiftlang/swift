// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s

class C {}

struct A {}
struct B { var owner: C }

var a = A()

// CHECK: assign {{%.*}} to {{%.*}} : $*A
// CHECK: release_value {{%.*}} : $B
(a, _) = (A(), B(owner: C()))
