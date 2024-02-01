// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip   -enable-experimental-feature NoncopyableGenerics -enable-builtin-module
// REQUIRES: noncopyable_generics

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

func derive(_ x: borrowing BufferView) -> _borrow(x) BufferView {
  return BufferView(x.ptr)
}

func consumeAndCreate(_ x: consuming BufferView) -> _copy(x) BufferView {
  return BufferView(x.ptr)
}

func use(_ x: borrowing BufferView) {}

func deriveMultiView1(_ x: borrowing BufferView, _ y: borrowing BufferView) -> _borrow(x, y) BufferView {
  if (Int.random(in: 1..<100) % 2 == 0) {
    return BufferView(x.ptr)
  }
  return BufferView(y.ptr)
}

func deriveMultiView2(_ x: borrowing BufferView, _ y: borrowing BufferView) -> _borrow(x) _borrow(y) BufferView {
  if (Int.random(in: 1..<100) % 2 == 0) {
    return BufferView(x.ptr)
  }
  return BufferView(y.ptr)
}

func consumeAndCreateMultiView1(_ x: consuming BufferView, _ y: consuming BufferView) -> _copy(x, y) BufferView {
  if (Int.random(in: 1..<100) % 2 == 0) {
    return BufferView(x.ptr)
  }
  return BufferView(y.ptr)
}

func consumeAndCreateMultiView2(_ x: consuming BufferView, _ y: consuming BufferView) -> _copy(x) _copy(y) BufferView {
  if (Int.random(in: 1..<100) % 2 == 0) {
    return BufferView(x.ptr)
  }
  return BufferView(y.ptr)
}

func mixedMultiView(_ x: consuming BufferView, _ y: borrowing BufferView) -> _copy(x) _borrow(y) BufferView {
  if (Int.random(in: 1..<100) % 2 == 0) {
    return BufferView(x.ptr)
  }
  return BufferView(y.ptr)
}

func modifiedViewDependsOnInput(_ x: inout MutableBufferView) -> _mutate(x) MutableBufferView {
  return MutableBufferView(x.ptr)
}

func modifiedViewDependsOnParent(_ x: inout MutableBufferView) -> _copy(x) MutableBufferView {
  return MutableBufferView(x.ptr)
}

func invalidSpecifier1(_ x: borrowing BufferView) -> _borrow BufferView { // expected-error{{expected '(' after lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func invalidSpecifier2(_ x: borrowing BufferView) -> _borrow() BufferView {// expected-error{{expected identifier, index or self in lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func invalidSpecifier3(_ x: borrowing BufferView) -> _borrow(*) BufferView { // expected-error{{expected identifier, index or self in lifetime dependence specifier}}
  return BufferView(x.ptr)
} 

// TODO: Diagnose using param indices on func decls in sema
func invalidSpecifier4(_ x: borrowing BufferView) -> _borrow(0) BufferView { // expected-error{{invalid lifetime dependence specifier, self is valid in non-static methods only}}
  return BufferView(x.ptr)
}

struct Wrapper : ~Escapable {
  let view: BufferView
  borrowing func getView1() -> _borrow(self) BufferView {
    return view
  }

  consuming func getView2() -> _consume(self) BufferView {
    return view
  }

  mutating func getView3() -> _copy(self) BufferView {
    return view
  }

  borrowing func getView4() -> _copy(self) BufferView {
    return view
  }
}
