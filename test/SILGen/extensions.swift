// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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

// Extensions of generic types with stored property initializers

// CHECK-LABEL: sil hidden [transparent] @_TIvV10extensions3Box1tGSqx_i : $@convention(thin) <T> () -> @out Optional<T>
// CHECK:      bb0(%0 : $*Optional<T>):
// CHECK:      [[FN:%.*]] = function_ref @_TFSqCfT10nilLiteralT__GSqx_ : $@convention(method) <τ_0_0> (@thin Optional<τ_0_0>.Type) -> @out Optional<τ_0_0>
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Optional<T>.Type
// CHECK-NEXT: apply [[FN]]<T>(%0, [[METATYPE]]) : $@convention(method) <τ_0_0> (@thin Optional<τ_0_0>.Type) -> @out Optional<τ_0_0>
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

struct Box<T> {
  let t: T? = nil
}

// CHECK-LABEL: sil hidden @_TFV10extensions3BoxCfT1tx_GS0_x_ : $@convention(method) <T> (@in T, @thin Box<T>.Type) -> @out Box<T>
// CHECK:      [[SELF_BOX:%.*]] = alloc_box $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK-NEXT: [[SELF_ADDR:%.*]] = project_box [[SELF_BOX]] : $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK:      [[SELF_PTR:%.*]] = mark_uninitialized [rootself] [[SELF_ADDR]] : $*Box<T>
// CHECK:      [[INIT:%.*]] = function_ref @_TIvV10extensions3Box1tGSqx_i : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<T>
// CHECK-NEXT: apply [[INIT]]<T>([[RESULT]]) : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
// CHECK-NEXT: [[T_ADDR:%.*]] = struct_element_addr [[SELF_PTR]] : $*Box<T>, #Box.t
// CHECK-NEXT: copy_addr [take] [[RESULT]] to [[T_ADDR]] : $*Optional<T>
// CHECK-NEXT: dealloc_stack [[RESULT]] : $*Optional<T>
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<T>
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: copy_addr %1 to [initialization] %14 : $*T
// CHECK-NEXT: inject_enum_addr [[RESULT]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: [[T_ADDR:%.*]] = struct_element_addr [[SELF_PTR]] : $*Box<T>, #Box.t
// CHECK-NEXT: copy_addr [take] [[RESULT]] to [[T_ADDR:%.*]] : $*Optional<T>
// CHECK-NEXT: dealloc_stack [[RESULT]] : $*Optional<T>
// CHECK-NEXT: copy_addr [[SELF_PTR]] to [initialization] %0 : $*Box<T>
// CHECK-NEXT: destroy_addr %1 : $*T
// CHECK-NEXT: destroy_value [[SELF_BOX]] : $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

extension Box {
  init(t: T) {
    self.t = t
  }
}
