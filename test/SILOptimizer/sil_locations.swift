// RUN: %target-swift-frontend -primary-file %s -emit-sil -emit-verbose-sil | %FileCheck %s

func searchForMe(_ x: Float) -> Float {
  return x
}

@_transparent func baz(_ x: Float) -> Float {
  return searchForMe(x);
}

@_transparent func bar(_ x: Float, _ b: Bool) -> Float {
  if b {
    return baz(x)
  }
  return x
  // CHECK-LABEL: $s13sil_locations3bar{{[_0-9a-zA-Z]*}}F
  // CHECK: function_ref @$s13sil_locations11searchForMe{{[_0-9a-zA-Z]*}}F : {{.*}} line:13:12:minlined
  // CHECK: apply {{.*}} line:13:12:minlined
}

func testMandatoryInlining(_ x: Float, b: Bool) -> Float {
  return bar(x, b)
// CHECK-LABEL: $s13sil_locations21testMandatoryInlining{{[_0-9a-zA-Z]*}}F
// CHECK: function_ref @$s13sil_locations11searchForMeyS2fF : {{.*}} line:22:10:minlined
// CHECK: apply                                                  {{.*}} line:22:10:minlined
// CHECK: br                                                     {{.*}} line:22:10:minlined
// CHECK: br                                                     {{.*}} line:22:10:minlined
}

