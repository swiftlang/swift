// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: | %FileCheck %s

// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

import Builtin

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSWcfC : $@convention(method) (UnsafeRawBufferPointer, @thin BufferView.Type) -> @lifetime(borrow 0)  @owned BufferView {
  @lifetime(borrow ptr)
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
  @_unsafeNonescapableResult
  init(independent ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
 // CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 1) @owned BufferView {
  @lifetime(borrow a)
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @thin BufferView.Type) -> @lifetime(copy 1)  @owned BufferView {
  @lifetime(a)
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper) {
    self.ptr = ptr
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVSaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(copy 1, borrow 2)  @owned BufferView {
  @lifetime(a, borrow b)
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper, _ b: borrowing Array<Int>) {
    self.ptr = ptr
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @lifetime(borrow ptr)
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
@lifetime(borrow x)
func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(independent: x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0)  @owned BufferView {
@lifetime(x)
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  return BufferView(independent: x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat1yAA10BufferViewVAD_ADtF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> @lifetime(copy 1, borrow 0)  @owned BufferView {
@lifetime(borrow this, that)
func deriveThisOrThat1(_ this: borrowing BufferView, _ that: borrowing BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(independent: this.ptr)
  }
  return BufferView(independent: that.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat2yAA10BufferViewVAD_ADntF : $@convention(thin) (@guaranteed BufferView, @owned BufferView) -> @lifetime(copy 1, borrow 0)  @owned BufferView {
@lifetime(borrow this, that)
func deriveThisOrThat2(_ this: borrowing BufferView, _ that: consuming BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(independent: this.ptr)
  }
  return BufferView(independent: that.ptr)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  let view: BufferView
  init(_ view: consuming BufferView) {
    self.view = view
  }
// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView1AA10BufferViewVyF : $@convention(method) (@guaranteed Wrapper) -> @lifetime(borrow 0)  @owned BufferView {
  @lifetime(borrow self)
  borrowing func getView1() -> BufferView {
    return view
  }

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView2AA10BufferViewVyF : $@convention(method) (@owned Wrapper) -> @lifetime(copy 0)  @owned BufferView {
  @lifetime(self)
  consuming func getView2() -> BufferView {
    return view
  }
}

struct Container : ~Escapable {
 let ptr: UnsafeRawBufferPointer
 @lifetime(borrow ptr)
 init(_ ptr: UnsafeRawBufferPointer) {
   self.ptr = ptr
 }
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getConsumingViewyAA06BufferG0VAA9ContainerVnF : $@convention(thin) (@owned Container) -> @lifetime(copy 0)  @owned BufferView {
@lifetime(x)
func getConsumingView(_ x: consuming Container) -> BufferView {
  return BufferView(independent: x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getBorrowingViewyAA06BufferG0VAA9ContainerVF : $@convention(thin) (@guaranteed Container) -> @lifetime(borrow 0)  @owned BufferView {
@lifetime(borrow x)
func getBorrowingView(_ x: borrowing Container) -> BufferView {
  return BufferView(independent: x.ptr)
}

