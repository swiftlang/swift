// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/def_explicit_lifetime_dependence.swift \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: -disable-lifetime-dependence-diagnostics

// RUN: %llvm-bcanalyzer %t/def_explicit_lifetime_dependence.swiftmodule 

// RUN: %target-swift-frontend -module-name lifetime-dependence -emit-sil -I %t %s \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

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

func testStaticMethod(
  inPlace message: inout MutableRawSpan,
  tag: RawSpan, gcm: GCM) {
  GCM.open(inPlace: &message, tag: tag)
  let propagatedMessage = GCM.propagate(message: message)
  let gcmX = gcm.x
  message = propagatedMessage
}

func testViewCallback(view: BufferView) {
  takeViewCallback { view }
}

// CHECK: sil @$s32def_explicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence15borrowAndCreateyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence16deriveThisOrThatyAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> @lifetime(copy 1, borrow 0) @owned BufferView
// CHECK: sil @$s32def_explicit_lifetime_dependence10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 1) @owned BufferView
// CHECK-LABEL: sil @$s32def_explicit_lifetime_dependence3GCMV4open7inPlace3tagys14MutableRawSpanVz_s0kL0VtFZ : $@convention(method) (@lifetime(copy 0) @inout MutableRawSpan, @guaranteed RawSpan, @thin GCM.Type) -> ()
// CHECK-LABEL: sil @$s32def_explicit_lifetime_dependence3GCMV9propagate7messages14MutableRawSpanVAGn_tFZ : $@convention(method) (@owned MutableRawSpan, @thin GCM.Type) -> @lifetime(copy 0) @owned MutableRawSpan
// CHECK-LABEL: sil @$s32def_explicit_lifetime_dependence3GCMV1xs7RawSpanVvg : $@convention(method) (GCM) -> @lifetime(borrow 0) @owned RawSpan
// CHECK: sil @$s32def_explicit_lifetime_dependence16takeViewCallback1fyAA06BufferF0VyXE_tF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @lifetime(captures) @owned BufferView)
