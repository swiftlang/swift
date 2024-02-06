// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics \
// RUN: -enable-experimental-lifetime-dependence-inference | %FileCheck %s
// REQUIRES: asserts
// REQUIRES: noncopyable_generics

import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2ChcfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> _borrow(1) @owned BufferView {
  init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2CcfC : $@convention(method) (@owned BufferView, @thin BufferView.Type) -> _inherit(1) @owned BufferView {
  init(_ otherBV: consuming BufferView) {
    self.ptr = otherBV.ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

/*
// rdar://121983770
func testBasic() {
  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBytes {
    let view = BufferView($0)
    let derivedView = derive(view)
    let newView = consumeAndCreate(derivedView)
    use(newView)    
  }
}
*/

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _borrow(1) @owned BufferView {
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr)
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  return BufferView(x.ptr)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  let view: BufferView
  init(_ view: consuming BufferView) {
    self.view = view
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> _borrow(0) @owned BufferView {
  borrowing func getView1() -> BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> BufferView {
    return view
  }
}
