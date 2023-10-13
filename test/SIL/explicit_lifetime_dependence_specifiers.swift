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

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> _borrow(1) @owned BufferView {
func derive(_ x: borrowing BufferView) -> _borrow(x) BufferView {
  return BufferView(x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> _copy(x) BufferView {
  return BufferView(x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat1yAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> _borrow(1, 2) @owned BufferView {
func deriveThisOrThat1(_ this: borrowing BufferView, _ that: borrowing BufferView) -> _borrow(this, that) BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this.ptr)
  }
  return BufferView(that.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat2yAA10BufferViewVAD_ADntF : $@convention(thin) (@guaranteed BufferView, @owned BufferView) -> _inherit(2)_borrow(1) @owned BufferView {
func deriveThisOrThat2(_ this: borrowing BufferView, _ that: consuming BufferView) -> _borrow(this) _copy(that) BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this.ptr)
  }
  return BufferView(that.ptr)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  let view: BufferView
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> _borrow(0) @owned BufferView {
  borrowing func getView1() -> _borrow(self) BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> _consume(self) BufferView {
    return view
  }
}
