// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

typealias Int = Builtin.Int64

var zero: Int

func call(f: () -> Int) -> Int {
  return f()
}

// CHECK: sil hidden @_TF17capture_typealias3fooFT_T_ : $@thin () -> () {
// CHECK: function_ref [[CLOSURE:@_TFF17capture_typealias3fooFT_T_U_FT_Bi64_]]
func foo() {
  typealias X = Int

  call {
    var x: X = zero
    return x
  }
}

// CHECK: sil shared @_TFF17capture_typealias3fooFT_T_U_FT_Bi64_ : $@thin () -> Builtin.Int64 {
