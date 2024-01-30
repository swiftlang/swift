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

// CHECK: sil @$s32def_explicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _borrow(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence15borrowAndCreateyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _borrow(1) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16deriveThisOrThatyAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> _borrow(1, 2) @owned BufferView
