// RUN: %swift -emit-sil %s | FileCheck %s

func foo(x:[objc_block] (Int) -> Int) {}

// CHECK: sil @bar
func bar() {
  var x = 0

  // CHECK: [[ANON_THIN:%[0-9]+]] = constant_ref $[thin] Int64 -> Int64, @<anonymous function>
  // CHECK: [[ANON_THICK:%[0-9]+]] = thin_to_thick_function [[ANON_THIN]]
  // CHECK: [[ANON_BLOCK:%[0-9]+]] = bridge_to_block [[ANON_THICK]]
  // CHECK: [[FOO:%[0-9]+]] = constant_ref ${{.*}}, @foo
  // CHECK: apply [[FOO]]([[ANON_BLOCK]])
  foo({$0})

  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply
  // CHECK: [[ANON_BLOCK:%[0-9]+]] = bridge_to_block [[ANON_CLOSURE]]
  // CHECK: [[FOO:%[0-9]+]] = constant_ref ${{.*}}, @foo
  // CHECK: apply [[FOO]]([[ANON_BLOCK]])
  foo({$0 + x})
}
