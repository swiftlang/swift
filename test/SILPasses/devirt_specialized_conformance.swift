// RUN: %swift -O3 %s -emit-sil | FileCheck %s

// Make sure that we completely inline/devirtualize/substitute all the way down
// to unknown1.

// CHECK-LABEL: sil private @top_level_code
// CHECK: bb0:
// CHECK-NEXT: global_addr
// CHECK-NEXT: alloc_ref
// CHECK-NEXT: store
// CHECK-NEXT: function_ref unknown1
// CHECK-NEXT: function_ref @unknown1
// CHECK-NEXT: apply
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@asmname("unknown1")
func unknown1() -> ()

protocol P {
  func doSomething()
}

struct X {}

class B<T> : P {
  func doSomething() {
     unknown1()
   }
 }

func doSomething(p : P) {
  p.doSomething()
}

var b = B<X>()
doSomething(b)
