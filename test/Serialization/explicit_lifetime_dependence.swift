// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/def_explicit_lifetime_dependence.swift \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics

// RUN: llvm-bcanalyzer %t/def_explicit_lifetime_dependence.swiftmodule 

// RUN: %target-swift-frontend -module-name lifetime-dependence -emit-silgen -I %t %s \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics | %FileCheck %s

// REQUIRES: noncopyable_generics

import def_explicit_lifetime_dependence

func testBasic() {
  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBytes {
    let view = BufferView($0)
    let derivedView = derive(view)
    let consumedView = consumeAndCreate(derivedView)
    let borrowedView = borrowAndCreate(consumedView) 
    let mysteryView = deriveThisOrThat(borrowedView, consumedView)
    use(mysteryView)
  }
}

func testInitializers() {
  let capacity = 4
  let a = Array(0..<capacity)
  let b = Array(0..<capacity)
  a.withUnsafeBytes {
    let view1 = BufferView($0, a)
    let view2 = BufferView($0, a, b)
    let mysteryView = deriveThisOrThat(view1, view2)
    use(mysteryView)
  }
}
// CHECK: sil @$s32def_explicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence15borrowAndCreateyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16deriveThisOrThatyAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> _scope(1, 2) @owned BufferView

// CHECK: sil @$s32def_explicit_lifetime_dependence10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> _scope(2) @owned BufferView

// CHECK: sil @$s32def_explicit_lifetime_dependence10BufferViewVyACSW_SaySiGADhtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Array<Int>, @guaranteed Array<Int>, @thin BufferView.Type) -> _inherit(2)_scope(3) @owned BufferView
