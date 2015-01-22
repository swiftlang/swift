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

  dynamic func kay() {}
  dynamic var cox: Int { return 0 }
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
  let _ = x.zippity

  // dynamic extension methods are still dynamically dispatched.
  // CHECK: class_method [volatile] %0 : $Foo, #Foo.kay!1.foreign
  x.kay()
  // CHECK: class_method [volatile] %0 : $Foo, #Foo.cox!getter.1.foreign
  let _ = x.cox

}

func extensionMethodCurrying(x: Foo) {
  let _ = x.zang
  let _ = x.kay
}

// CHECK-LABEL: sil shared @_TFC10extensions3Foo4zangFS0_FT_T_
// CHECK:         function_ref @_TFC10extensions3Foo4zangfS0_FT_T_
// CHECK:       sil shared @_TFC10extensions3Foo3kayFS0_FT_T_
// CHECK:         function_ref @_TTDFC10extensions3Foo3kayfS0_FT_T_
// CHECK:       sil shared [transparent] @_TTDFC10extensions3Foo3kayfS0_FT_T_
// CHECK:         class_method [volatile] %0 : $Foo, #Foo.kay!1.foreign
