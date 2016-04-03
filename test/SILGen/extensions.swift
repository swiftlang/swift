// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class Foo {
  // CHECK-LABEL: sil hidden @_TFC10extensions3Foo3zim
  func zim() {}
}

extension Foo {
  // CHECK-LABEL: sil hidden @_TFC10extensions3Foo4zang
  func zang() {}

  // CHECK-LABEL: sil hidden @_TFC10extensions3Foog7zippitySi
  var zippity: Int { return 0 }
}

struct Bar {
  // CHECK-LABEL: sil hidden @_TFV10extensions3Bar4zung
  func zung() {}
}

extension Bar {
  // CHECK-LABEL: sil hidden @_TFV10extensions3Bar4zoom
  func zoom() {}
}

// CHECK-LABEL: sil hidden @_TF10extensions19extensionReferencesFCS_3FooT_
func extensionReferences(_ x: Foo) {
  // Non-objc extension methods are statically dispatched.
  // CHECK: function_ref @_TFC10extensions3Foo4zang
  x.zang()
  // CHECK: function_ref @_TFC10extensions3Foog7zippitySi
  _ = x.zippity

}

func extensionMethodCurrying(_ x: Foo) {
  _ = x.zang
}

// CHECK-LABEL: sil shared [thunk] @_TFC10extensions3Foo4zang
// CHECK:         function_ref @_TFC10extensions3Foo4zang
