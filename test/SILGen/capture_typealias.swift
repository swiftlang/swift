// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s | %FileCheck %s

typealias Int = Builtin.Int64

var zero: Int

func call(f: () -> Int) -> Int {
  return f()
}

// CHECK: sil hidden @$s17capture_typealias3fooyyF : $@convention(thin) () -> () {
// CHECK: function_ref [[CLOSURE:@\$s17capture_typealias3fooyyFBi64_yXEfU_]]
func foo() {
  typealias X = Int

  call {
    var x: X = zero
    return x
  }
}

// CHECK: sil private @$s17capture_typealias3fooyyFBi64_yXEfU_ : $@convention(thin) () -> Builtin.Int64 {
