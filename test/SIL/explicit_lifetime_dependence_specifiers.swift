// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -enable-experimental-feature NoncopyableGenerics | %FileCheck %s


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
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_SaySiGhYlstcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> _scope(2) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) -> dependsOn(a) Self {
    self.ptr = ptr
    return self
  }
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVYlitcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @thin BufferView.Type) -> _inherit(2) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper) -> dependsOn(a) Self {
    self.ptr = ptr
    return self
  }
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers10BufferViewVyACSW_AA7WrapperVYliSaySiGhYlstcfC : $@convention(method) (UnsafeRawBufferPointer, @owned Wrapper, @guaranteed Array<Int>, @thin BufferView.Type) -> _inherit(2) _scope(3) @owned BufferView {
  init(_ ptr: UnsafeRawBufferPointer, _ a: consuming Wrapper, _ b: borrowing Array<Int>) -> dependsOn(a) dependsOn(b) Self {
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

// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers6deriveyAA10BufferViewVADYlsF : $@convention(thin) (@guaranteed BufferView) -> _scope(1) @owned BufferView {
func derive(_ x: borrowing BufferView) -> dependsOn(scoped x) BufferView {
  return BufferView(x.ptr)
}

// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers16consumeAndCreateyAA10BufferViewVADnYliF : $@convention(thin) (@owned BufferView) -> _inherit(1) @owned BufferView {
func consumeAndCreate(_ x: consuming BufferView) -> dependsOn(x) BufferView {
  return BufferView(x.ptr)
}

// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat1yAA10BufferViewVADYls_ADYlstF : $@convention(thin) (@guaranteed BufferView, @guaranteed BufferView) -> _scope(1, 2) @owned BufferView {
func deriveThisOrThat1(_ this: borrowing BufferView, _ that: borrowing BufferView) -> dependsOn(scoped this, that) BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this.ptr)
  }
  return BufferView(that.ptr)
}

// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers17deriveThisOrThat2yAA10BufferViewVADYls_ADnYlitF : $@convention(thin) (@guaranteed BufferView, @owned BufferView) -> _inherit(2) _scope(1) @owned BufferView {
func deriveThisOrThat2(_ this: borrowing BufferView, _ that: consuming BufferView) -> dependsOn(scoped this) dependsOn(that) BufferView {
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
// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView1AA10BufferViewVyYLsF : $@convention(method) (@guaranteed Wrapper) -> _scope(0) @owned BufferView {
  borrowing func getView1() -> dependsOn(scoped self) BufferView {
    return view
  }

// CHECK: sil hidden @$s39explicit_lifetime_dependence_specifiers7WrapperV8getView2AA10BufferViewVyYLiF : $@convention(method) (@owned Wrapper) -> _inherit(0) @owned BufferView {
  consuming func getView2() -> dependsOn(self) BufferView {
    return view
  }
}

struct Container : ~Escapable {
 let ptr: UnsafeRawBufferPointer
 @_unsafeNonescapableResult
 init(_ ptr: UnsafeRawBufferPointer) {
   self.ptr = ptr
 }
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getConsumingViewyAA06BufferG0VAA9ContainerVnYliF : $@convention(thin) (@owned Container) -> _inherit(1) @owned BufferView {
func getConsumingView(_ x: consuming Container) -> dependsOn(x) BufferView {
  return BufferView(x.ptr)
}

// CHECK-LABEL: sil hidden @$s39explicit_lifetime_dependence_specifiers16getBorrowingViewyAA06BufferG0VAA9ContainerVYlsF : $@convention(thin) (@guaranteed Container) -> _scope(1) @owned BufferView {
func getBorrowingView(_ x: borrowing Container) -> dependsOn(scoped x) BufferView {
  return BufferView(x.ptr)
}

