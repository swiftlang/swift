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
  @_unsafeNonescapableResult
  init?(_ ptr: UnsafeRawBufferPointer, _ i: Int) {
    if (i % 2 == 0) {
      return nil
    } 
    self.ptr = ptr
  }
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> _borrow(2) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) -> _borrow(a) Self {
    self.ptr = ptr
    return self
  }
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySdGtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Array<Double>, @thin BufferView.Type) -> _inherit(2) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Array<Double>) -> _consume(a) Self {
    self.ptr = ptr
    return self
  }
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySdGSaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Array<Double>, @guaranteed Array<Int>, @thin BufferView.Type) -> _inherit(2)_borrow(3) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Array<Double>, _ b: borrowing Array<Int>) -> _consume(a) _borrow(b) Self {
    self.ptr = ptr
    return self
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
  init(_ view: consuming BufferView) {
    self.view = view
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> _borrow(0) @owned BufferView {
  borrowing func getView1() -> _borrow(self) BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> _consume(self) BufferView {
    return view
  }
}
