// RUN: %swift -emit-silgen %s | FileCheck %s

func call(f : () -> Int) -> Int {
  return f()
}

// CHECK: sil @_T17capture_typealias3fooFT_T_ : $@thin () -> () {
// CHECK: function_ref [[CLOSURE:@closure[0-9]+]]
func foo() {
  typealias X = Int

  call {
    var x: X = 0
    return x
  }
}

// CHECK: sil internal @closure0 : $@thin () -> Int64 {
