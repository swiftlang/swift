// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck %s

typealias Int = Builtin.Int64

var zero: Int

func call(f: () -> Int) -> Int {
  return f()
}

// CHECK: sil hidden @_T017capture_typealias3fooyyF : $@convention(thin) () -> () {
// CHECK: function_ref [[CLOSURE:@_T017capture_typealias3fooyyFBi64_ycfU_]]
func foo() {
  typealias X = Int

  call {
    var x: X = zero
    return x
  }
}

// CHECK: sil shared @_T017capture_typealias3fooyyFBi64_ycfU_ : $@convention(thin) () -> Builtin.Int64 {
