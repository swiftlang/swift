// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -disable-arc-opts -emit-sil %s | %FileCheck %s

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

//CHECK: sil shared [noinline] {{.*}}@$s10spec_conf114outer_function2Inyx_tAA1PRzlFAA3FooC_Ttg5
//CHECK: $s10spec_conf114inner_function2Inyx_tAA1PRzlFAA3FooC_Ttg5
//CHECK-NEXT: apply
//CHECK: return

outer_function(In: Foo())
