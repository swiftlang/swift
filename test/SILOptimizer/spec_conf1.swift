// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -Xllvm -sil-disable-pass="Function Signature Optimization" -disable-arc-opts -emit-sil -Xllvm -enable-destroyhoisting=false %s | %FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

protocol P { func p() }
protocol Q { func q() }

class Foo: P, Q {
  @inline(never)
  func p() {}
  @inline(never)
  func q() {}
}

@inline(never)
func inner_function<T : P>(In In : T) { }

@inline(never)
func outer_function<T : P>(In In : T) { inner_function(In: In) }

//CHECK: sil shared [noinline] @_T010spec_conf114outer_functionyx2In_tAA1PRzlFAA3FooC_Tg5
//CHECK: _T010spec_conf114inner_functionyx2In_tAA1PRzlFAA3FooC_Tg5
//CHECK-NEXT: apply
//CHECK: return

outer_function(In: Foo())
