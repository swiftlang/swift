// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

class Foo {
  // CHECK-LABEL: sil hidden @$s10extensions3FooC3zim{{[_0-9a-zA-Z]*}}F
  func zim() {}
}

extension Foo {
  // CHECK-LABEL: sil hidden @$s10extensions3FooC4zang{{[_0-9a-zA-Z]*}}F
  func zang() {}

  // CHECK-LABEL: sil hidden @$s10extensions3FooC7zippitySivg
  var zippity: Int { return 0 }
}

struct Bar {
  // CHECK-LABEL: sil hidden @$s10extensions3BarV4zung{{[_0-9a-zA-Z]*}}F
  func zung() {}
}

extension Bar {
  // CHECK-LABEL: sil hidden @$s10extensions3BarV4zoom{{[_0-9a-zA-Z]*}}F
  func zoom() {}
}

// CHECK-LABEL: sil hidden @$s10extensions19extensionReferencesyyAA3FooCF
func extensionReferences(_ x: Foo) {
  // Non-objc extension methods are statically dispatched.
  // CHECK: function_ref @$s10extensions3FooC4zang{{[_0-9a-zA-Z]*}}F
  x.zang()
  // CHECK: function_ref @$s10extensions3FooC7zippitySivg
  _ = x.zippity

}

func extensionMethodCurrying(_ x: Foo) {
  _ = x.zang
}

// CHECK-LABEL: sil shared [thunk] @$s10extensions3FooC4zang{{[_0-9a-zA-Z]*}}F
// CHECK:         function_ref @$s10extensions3FooC4zang{{[_0-9a-zA-Z]*}}F

// Extensions of generic types with stored property initializers

// CHECK-LABEL: sil hidden [transparent] @$s10extensions3BoxV1txSgvpfi : $@convention(thin) <T> () -> @out Optional<T>
// CHECK:      bb0(%0 : $*Optional<T>):
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Optional<T>.Type
// CHECK:      inject_enum_addr %0 : $*Optional<T>, #Optional.none!enumelt
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

struct Box<T> {
  let t: T? = nil
}

// CHECK-LABEL: sil hidden @$s10extensions3BoxV1tACyxGx_tcfC : $@convention(method) <T> (@in T, @thin Box<T>.Type) -> @out Box<T>
// CHECK:      [[SELF_BOX:%.*]] = alloc_box $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK-NEXT: [[UNINIT_SELF_BOX:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]
// CHECK-NEXT: [[SELF_ADDR:%.*]] = project_box [[UNINIT_SELF_BOX]] : $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK:      [[INIT:%.*]] = function_ref @$s10extensions3BoxV1txSgvpfi : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<T>
// CHECK-NEXT: apply [[INIT]]<T>([[RESULT]]) : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
// CHECK-NEXT: [[T_ADDR:%.*]] = struct_element_addr [[SELF_ADDR]] : $*Box<T>, #Box.t
// CHECK-NEXT: copy_addr [take] [[RESULT]] to [[T_ADDR]] : $*Optional<T>
// CHECK-NEXT: dealloc_stack [[RESULT]] : $*Optional<T>
// CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<T>
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: copy_addr %1 to [initialization] %14 : $*T
// CHECK-NEXT: inject_enum_addr [[RESULT]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[SELF_ADDR]] : $*Box<T>
// CHECK-NEXT: [[T_ADDR:%.*]] = struct_element_addr [[WRITE]] : $*Box<T>, #Box.t
// CHECK-NEXT: copy_addr [take] [[RESULT]] to [[T_ADDR:%.*]] : $*Optional<T>
// CHECK-NEXT: end_access [[WRITE]]
// CHECK-NEXT: dealloc_stack [[RESULT]] : $*Optional<T>
// CHECK-NEXT: copy_addr [[SELF_ADDR]] to [initialization] %0 : $*Box<T>
// CHECK-NEXT: destroy_addr %1 : $*T
// CHECK-NEXT: destroy_value [[UNINIT_SELF_BOX]] : $<τ_0_0> { var Box<τ_0_0> } <T>
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

extension Box {
  init(t: T) {
    self.t = t
  }
}
