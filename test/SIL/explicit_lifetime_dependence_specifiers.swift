// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSWcfC : $@convention(method) (UnsafeRawBufferPointer, @thin BufferView.Type) -> @lifetime(borrow 0)  @owned BufferView {
  @_lifetime(borrow ptr)
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
  @_lifetime(borrow ptr)
  init(independent ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
 // CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 1) @owned BufferView {
  @_lifetime(borrow a)
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @thin BufferView.Type) -> @lifetime(copy 1)  @owned BufferView {
  @_lifetime(copy a)
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper) {
    self.ptr = ptr
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVSaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(copy 1, borrow 2)  @owned BufferView {
  @_lifetime(copy a, borrow b)
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper, _ b: borrowing Array<Int>) {
    self.ptr = ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_lifetime(borrow ptr)
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

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(borrow 0)  @owned BufferView {
@_lifetime(borrow x)
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(independent: x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0)  @owned BufferView {
@_lifetime(copy x)
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  let bv = BufferView(independent: x.ptr)
  return _overrideLifetime(bv, copying: x)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat1yAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> @lifetime(copy 1, borrow 0)  @owned BufferView {
@_lifetime(borrow this, copy that)
func deriveThisOrThat1(_ this: borrowing BufferView, _ that: borrowing BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(independent: this.ptr)
  }
  let newThat = BufferView(independent: that.ptr)
  return _overrideLifetime(newThat, copying: that)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat2yAA10BufferViewVAD_ADntF : $@convention(thin) (@guaranteed BufferView, @owned BufferView) -> @lifetime(copy 1, borrow 0)  @owned BufferView {
@_lifetime(borrow this, copy that)
func deriveThisOrThat2(_ this: borrowing BufferView, _ that: consuming BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(independent: this.ptr)
  }
  let bv = BufferView(independent: that.ptr)
  return _overrideLifetime(bv, copying: that)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  let view: BufferView
  @_lifetime(copy view)
  init(_ view: consuming BufferView) {
    self.view = view
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> @lifetime(borrow 0)  @owned BufferView {
  @_lifetime(borrow self)
  borrowing func getView1() -> BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> @lifetime(copy 0)  @owned BufferView {
  @_lifetime(copy self)
  consuming func getView2() -> BufferView {
    return view
  }
}

struct Container : ~Escapable {
 let ptr: UnsafeRawBufferPointer
 @_lifetime(borrow ptr)
 init(_ ptr: UnsafeRawBufferPointer) {
   self.ptr = ptr
 }
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getConsumingViewyAA06BufferG0VAA9ContainerVnF : $@convention(thin) (@owned Container) -> @lifetime(copy 0)  @owned BufferView {
@_lifetime(copy x)
func getConsumingView(_ x: consuming Container) -> BufferView {
  let bv = BufferView(independent: x.ptr)
  return _overrideLifetime(bv, copying: x)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getBorrowingViewyAA06BufferG0VAA9ContainerVF : $@convention(thin) (@guaranteed Container) -> @lifetime(borrow 0)  @owned BufferView {
@_lifetime(borrow x)
func getBorrowingView(_ x: borrowing Container) -> BufferView {
  return BufferView(independent: x.ptr)
}
