// RUN: %swift %s -emit-sil -emit-verbose-sil | FileCheck %s

func searchForMe(x : Float) -> Float {
  return x
}

func [transparent] baz(x : Float) -> Float {
  return searchForMe(x);
}

func [transparent] bar(x : Float, b: Bool) -> Float {
  if b {
    return baz(x)
  }
  return x
  // CHECK-LABEL: _T13sil_locations3barFT1xSf1bSb_Sf
  // CHECK: function_ref @_T13sil_locations11searchForMeFT1xSf_Sf {{.*}} line:13:15:inlined
  // CHECK: apply {{.*}} line:13:15:inlined
}

func testMandatoryInlining(x : Float, b : Bool) -> Float {
  return bar(x, b);
// CHECK-LABEL: _T13sil_locations21testMandatoryInliningFT1xSf1bSb_Sf
// CHECK: alloc_stack {{.*}} line:21:28
// CHECK: function_ref @_T13sil_locations11searchForMeFT1xSf_Sf {{.*}} line:22:13:inlined
// CHECK: apply                                                 {{.*}} line:22:13:inlined
// CHECK: br                                                    {{.*}} line:22:13:inlined
// CHECK: br                                                    {{.*}} line:22:13:inlined
}

