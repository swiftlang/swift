// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class Foo {
  // CHECK-LABEL: sil hidden @_TFC10extensions3Foo3zimfS0_FT_T_
  func zim() {}
}

extension Foo {
  // CHECK-LABEL: sil hidden @_TFC10extensions3Foo4zangfS0_FT_T_
  func zang() {}

  // CHECK-LABEL: sil hidden @_TFC10extensions3Foog7zippitySi
  var zippity: Int { return 0 }
}

struct Bar {
  // CHECK-LABEL: sil hidden @_TFV10extensions3Bar4zungfS0_FT_T_
  func zung() {}
}

extension Bar {
  // CHECK-LABEL: sil hidden @_TFV10extensions3Bar4zoomfS0_FT_T_
  func zoom() {}
}

// CHECK-LABEL: sil hidden @_TF10extensions19extensionReferencesFCS_3FooT_
func extensionReferences(x: Foo) {
  // Non-objc extension methods are statically dispatched.
  // CHECK: function_ref @_TFC10extensions3Foo4zangfS0_FT_T_
  x.zang()
  // CHECK: function_ref @_TFC10extensions3Foog7zippitySi
  _ = x.zippity

}

func extensionMethodCurrying(x: Foo) {
  _ = x.zang
}

// CHECK-LABEL: sil shared @_TFC10extensions3Foo4zangFS0_FT_T_
// CHECK:         function_ref @_TFC10extensions3Foo4zangfS0_FT_T_
