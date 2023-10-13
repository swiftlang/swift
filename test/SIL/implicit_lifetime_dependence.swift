// RUN: %target-swift-frontend %s -emit-sil  -enable-builtin-module \
// RUN:   -Xllvm -disable-lifetime-dependence-diagnostics \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   | %FileCheck %s

// REQUIRES: asserts

import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

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
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> _borrow(0) @owned BufferView {
  borrowing func getView1() -> BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> BufferView {
    return view
  }
}

