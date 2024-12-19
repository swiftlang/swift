// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/def_explicit_lifetime_dependence.swift \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: -disable-lifetime-dependence-diagnostics

// RUN: llvm-bcanalyzer %t/def_explicit_lifetime_dependence.swiftmodule 

// RUN: %target-swift-frontend -module-name lifetime-dependence -emit-sil -I %t %s \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_LifetimeDependence

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
    let view2 = BufferView($0, b)
    let mysteryView = deriveThisOrThat(view1, view2)
    use(mysteryView)
  }
}

func testReadAccessor() {
  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBytes {
    let view = BufferView($0, a)
    let w = Wrapper(view)
    use(w.view)
  }
}

func testFakeOptional() {
  _ = FakeOptional<Int>(())
}

// CHECK: sil @$s32def_explicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence15borrowAndCreateyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16deriveThisOrThatyAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> @lifetime(copy 1, borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 1) @owned BufferView
