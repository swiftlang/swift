// RUN: %target-swift-frontend -O -Xllvm -sil-inline-generics=false %s -emit-sil -sil-verify-all | %FileCheck %s

// Make sure that we completely inline/devirtualize/substitute all the way down
// to unknown1.

// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: alloc_ref
// CHECK: function_ref @unknown1
// CHECK: apply
// CHECK: apply
// CHECK: return

struct Int32 {}

@_silgen_name("unknown1")
func unknown1() -> ()

protocol P {
  func doSomething(_ x : Int32)
}

struct X {}

class B<T> : P {
  func doSomething(_ x : Int32) {
     unknown1()
   }
 }

func doSomething(_ p : P, _ x : Int32) {
  p.doSomething(x)
}
func doSomething2<T : P>(_ t : T, _ x : Int32) {
  t.doSomething(x)
}

func driver() {
  var b2 = B<X>()
  var x = Int32()
  doSomething(b2, x)
  doSomething2(b2, x)
}

driver()
