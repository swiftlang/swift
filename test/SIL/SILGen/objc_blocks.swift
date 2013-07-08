// RUN: %swift -emit-sil %s | FileCheck %s

func foo(x:[objc_block] (Int) -> Int) {}

// CHECK: sil @_T11objc_blocks3barFT_T_
func bar() {
  var x = 0

  // CHECK: [[FOO:%[0-9]+]] = function_ref @_T11objc_blocks3fooFT1xbSiSi_T_
  // CHECK: [[ANON_THIN:%[0-9]+]] = function_ref @closure{{.*}} : $[thin] ($0 : Int64) -> Int64
  // CHECK: [[ANON_THIN_CONV:%[0-9]+]] = convert_function [[ANON_THIN]]
  // CHECK: [[ANON_THICK:%[0-9]+]] = thin_to_thick_function [[ANON_THIN_CONV]]
  // CHECK: [[ANON_BLOCK:%[0-9]+]] = bridge_to_block [[ANON_THICK]]
  // CHECK: apply [[FOO]]([[ANON_BLOCK]])
  foo({$0})

  // CHECK: [[FOO:%[0-9]+]] = function_ref @_T11objc_blocks3fooFT1xbSiSi_T_
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply
  // CHECK: [[ANON_CLOSURE_CONV:%[0-9]+]] = convert_function [[ANON_CLOSURE]]
  // CHECK: [[ANON_BLOCK:%[0-9]+]] = bridge_to_block [[ANON_CLOSURE_CONV]]
  // CHECK: apply [[FOO]]([[ANON_BLOCK]])
  foo({$0 + x})
}
