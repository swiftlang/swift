// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// rdar://problem/67781123 - crash in SILGen

struct Foo {
  var title: String
  var handler1: ((Int, String) -> Void)?
  var handler2: (() -> Void)?
}

func take(foo: Foo) { }

// CHECK-LABEL: sil hidden [ossa] @$s42forward_trailing_closure_unresolved_member4testyy
func test() {
  // CHECK: function_ref @$s42forward_trailing_closure_unresolved_member4testyyFyycfU_ : $@convention(thin) () -> ()
  take(foo: .init(title: "") {
      print("handler2 is called")
    })
}
