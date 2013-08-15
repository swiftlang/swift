// RUN: %swift %s -emit-sil -o - -verify | FileCheck %s

// These tests are deliberately shallow, because I do not want to depend on the
// specifics of SIL generation, which might change for reasons unrelated to this
// pass

func foo(x : Float) -> Float {
  return bar(x);
}

// CHECK-LABEL: sil @_T18mandatory_inlining3fooFT1xSf_Sf
// CHECK-NOT: function_ref
// CHECK-NOT: apply

func [force_inline] bar(x : Float) -> Float {
  return baz(x)
}

// CHECK-LABEL: sil @_T18mandatory_inlining3barFT1xSf_Sf
// CHECK-NOT: function_ref
// CHECK-NOT: apply

func [force_inline] baz(x : Float) -> Float {
  return x;
}

// CHECK-LABEL: sil @_T18mandatory_inlining3bazFT1xSf_Sf

func spam(x : Int) -> Int {
  return x
}

// CHECK-LABEL: sil @_T18mandatory_inlining4spamFT1xSi_Si

func [force_inline] ham(x : Int) -> Int {
  return spam(x)
}

// CHECK-LABEL: sil @_T18mandatory_inlining3hamFT1xSi_Si
// CHECK: function_ref @_T18mandatory_inlining4spamFT1xSi_Si
// CHECK: apply

func eggs(x : Int) -> Int {
  return ham(x)
}

// CHECK-LABEL: sil @_T18mandatory_inlining4eggsFT1xSi_Si
// CHECK: function_ref @_T18mandatory_inlining4spamFT1xSi_Si
// CHECK: apply
