// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

class Foo {
  // CHECK-LABEL: sil hidden [ossa] @$s30dynamic_self_reference_storage3FooC0A4Self{{[_0-9a-zA-Z]*}}F
  func dynamicSelf() -> Self {
    // CHECK: debug_value {{%.*}} : $Foo
    let managedSelf = self
    // CHECK: alloc_box ${ var @sil_unmanaged Foo }
    unowned(unsafe) let unmanagedSelf = self
    // CHECK: alloc_box ${ var @sil_unowned Foo }
    unowned(safe) let unownedSelf = self
    // CHECK: alloc_box ${ var @sil_weak Optional<Foo> }
    weak var weakSelf = self
    // CHECK: debug_value {{%.*}} : $Optional<Foo>
    let optionalSelf = Optional(self)

    // CHECK: alloc_box ${ var @sil_unowned Foo }
    let f: () -> () = {[unowned self] in ()}
    // CHECK: alloc_box ${ var @sil_weak Optional<Foo> }
    let g: () -> () = {[weak self] in ()}
  }
}
