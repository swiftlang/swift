// RUN: %swift -emit-silgen %s | FileCheck %s

class Foo {
  // CHECK-LABEL: sil  @_TFC10extensions3Foo3zimfS0_FT_T_
  func zim() {}
}

extension Foo {
  // CHECK-LABEL: sil  @_TFC10extensions3Foo4zangfS0_FT_T_
  func zang() {}

  // CHECK-LABEL: sil  @_TFC10extensions3Foog7zippitySi
  var zippity: Int { return 0 }

  @objc func kay() {}
  @objc var cox: Int { return 0 }
}

struct Bar {
  // CHECK-LABEL: sil  @_TFV10extensions3Bar4zungfS0_FT_T_
  func zung() {}
}

extension Bar {
  // CHECK-LABEL: sil  @_TFV10extensions3Bar4zoomfS0_FT_T_
  func zoom() {}
}

// CHECK-LABEL: sil @_TF10extensions19extensionReferencesFCS_3FooT_
func extensionReferences(x: Foo) {
  // Non-objc extension methods are statically dispatched.
  // CHECK: function_ref @_TFC10extensions3Foo4zangfS0_FT_T_
  x.zang()
  // CHECK: function_ref @_TFC10extensions3Foog7zippitySi
  let _ = x.zippity

  // @objc extension methods are still dynamically dispatched.
  // CHECK: function_ref @_TFC10extensions3Foo3kayfS0_FT_T_ : $@cc(method) @thin (@owned Foo) -> ()
  x.kay()
  // CHECK: function_ref @_TFC10extensions3Foog3coxSi : $@cc(method) @thin (@owned Foo) -> Int
  let _ = x.cox

}
