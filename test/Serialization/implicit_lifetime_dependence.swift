// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/def_implicit_lifetime_dependence.swift \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics

// RUN: llvm-bcanalyzer %t/def_implicit_lifetime_dependence.swiftmodule 

// RUN: %target-swift-frontend -module-name lifetime-dependence -emit-silgen -I %t %s \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics \
// RUN: -enable-experimental-lifetime-dependence-inference | %FileCheck %s

// REQUIRES: noncopyable_generics

import def_implicit_lifetime_dependence

func testBasic() {
  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBytes {
    let view = BufferView($0)
    let derivedView = derive(view)
    let consumedView = consumeAndCreate(derivedView)
    let borrowedView = borrowAndCreate(consumedView) 
    use(borrowedView)
  }
}

func testInitializers() {
  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBytes {
    let view1 = BufferView($0)
    let view2 = BufferView(view1)
    let view3 = BufferView(view2)
    use(view3)
  }
}

func unsafetest(_ ptr: UnsafeRawBufferPointer) {
  let view1 = BufferView(ptr)
  let view2 = BufferView(view1)
  let view3 = BufferView(view2)
  use(view3)
}

// CHECK: sil @$s32def_implicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView
// CHECK: sil @$s32def_implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView
// CHECK: sil @$s32def_implicit_lifetime_dependence15borrowAndCreateyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView
// CHECK: sil @$s32def_implicit_lifetime_dependence10BufferViewVyA2ChcfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> _scope(1) @owned BufferView

