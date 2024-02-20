// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics \
// RUN: -enable-experimental-lifetime-dependence-inference | %FileCheck %s
// REQUIRES: asserts


import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2ChYlscfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> _scope(1) @owned BufferView {
  init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2CYlicfC : $@convention(method) (@owned BufferView, @thin BufferView.Type) -> _inherit(1) @owned BufferView {
  init(_ otherBV: consuming BufferView) {
    self.ptr = otherBV.ptr
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACSWYls_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> _scope(1) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_unsafeNonescapableResult
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

// CHECK: sil hidden @$s28implicit_lifetime_dependence6deriveyAA10BufferViewVADYlsF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView {
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr)
}

// CHECK: sil hidden @$s28implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnYliF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  return BufferView(x.ptr)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  let view: BufferView
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperVyAcA10BufferViewVYlicfC : $@convention(method) (@owned BufferView, @thin Wrapper.Type) -> _inherit(1) @owned Wrapper {
  init(_ view: consuming BufferView) {
    self.view = view
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVyYLsF : $@convention(method) (@guaranteed Wrapper) -> _scope(0) @owned BufferView {
  borrowing func getView1() -> BufferView {
    return view
  }

// CHECK:sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVyYLiF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> BufferView {
    return view
  }
}

struct Container : ~Copyable {
  var ptr: UnsafeRawBufferPointer
// CHECK: sil hidden @$s28implicit_lifetime_dependence9ContainerV4viewAA10BufferViewVvg : $@convention(method) (@guaranteed Container) -> _scope(0) @owned BufferView {
  var view: BufferView {
    get {
      return BufferView(ptr)
    }
    set(newView) {
      ptr = newView.ptr
    }
  }
}
