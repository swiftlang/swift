// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Foo {
  // CHECK-LABEL: sil hidden @_TFC30dynamic_self_reference_storage3Foo11dynamicSelf
  func dynamicSelf() -> Self {
    // CHECK: debug_value {{%.*}} : $Foo
    let managedSelf = self
    // CHECK: alloc_box $@sil_unmanaged Foo
    unowned(unsafe) let unmanagedSelf = self
    // CHECK: alloc_box $@sil_unowned Foo
    unowned(safe) let unownedSelf = self
    // CHECK: alloc_box $@sil_weak Optional<Foo>
    weak var weakSelf = self
    // CHECK: debug_value {{%.*}} : $Optional<Foo>
    let optionalSelf = Optional(self)

    // CHECK: alloc_box $@sil_unowned Foo
    let f: () -> () = {[unowned self] in ()}
    // CHECK: alloc_box $@sil_weak Optional<Foo>
    let g: () -> () = {[weak self] in ()}
  }
}
