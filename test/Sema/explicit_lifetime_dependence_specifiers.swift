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

class Klass {}

struct WrapperStruct {
  let k: Klass
}

func invalidLifetimeDependenceOnEscapableResult(_ w: borrowing WrapperStruct) -> _borrow(w) Klass { // expected-error{{lifetime dependence can only be specified on ~Escapable results}}
  return w.k
}

func incorrectSelfInvalidLifetimeDependence(_ x: borrowing BufferView) -> _borrow(self) BufferView { // expected-error{{invalid lifetime dependence specifier, self is valid in non-static methods only}}
  return BufferView(x.ptr)
}

func incorrectParamNameInvalidLifetimeDependence(_ x: borrowing BufferView) -> _borrow(y) BufferView { // expected-error{{invalid parameter name specified 'y'}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence1(_ x: borrowing BufferView) -> _borrow(x, x) BufferView { // expected-error{{duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence2(_ x: borrowing BufferView) -> _borrow(x) _borrow(x) BufferView { // expected-error{{duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func duplicateParamInvalidLifetimeDependence3(_ x: borrowing BufferView) -> _borrow(x) _copy(x) BufferView { // expected-error{{duplicate lifetime dependence specifier}}
  return BufferView(x.ptr)
}

func consumingParamInvalidLifetimeDependence1(_ x: consuming BufferView) -> _borrow(x) BufferView { // expected-error{{invalid use of borrow lifetime dependence for consuming ownership}}
  return BufferView(x.ptr)
}

func consumingParamInvalidLifetimeDependence2(_ x: consuming BufferView) -> _mutate(x) BufferView { // expected-error{{invalid use of mutate lifetime dependence for consuming ownership}}
  return BufferView(x.ptr)
}

func borrowingParamInvalidLifetimeDependence1(_ x: borrowing BufferView) -> _consume(x) BufferView { // expected-error{{invalid use of consume lifetime dependence for borrowing ownership}}
  return BufferView(x.ptr)
}

func borrowingParamInvalidLifetimeDependence2(_ x: borrowing BufferView) -> _mutate(x) BufferView { // expected-error{{invalid use of mutate lifetime dependence for borrowing ownership}}
  return BufferView(x.ptr)
}

func implicitBorrowingParamInvalidLifetimeDependence1(_ x: BufferView) -> _consume(x) BufferView { // expected-error{{lifetime dependence can only be specified on parameters with ownership modifiers (borrowing, consuming, inout)}}
  return BufferView(x.ptr)
}

func implicitBorrowingParamInvalidLifetimeDependence2(_ x: BufferView) -> _mutate(x) BufferView {// expected-error{{lifetime dependence can only be specified on parameters with ownership modifiers (borrowing, consuming, inout)}}
  return BufferView(x.ptr)
}

func inoutParamInvalidLifetimeDependence1(_ x: inout BufferView) -> _consume(x) BufferView { // expected-error{{invalid use of consume lifetime dependence for inout ownership}}
  return BufferView(x.ptr)
}

func inoutParamInvalidLifetimeDependence2(_ x: inout BufferView) -> _borrow(x) BufferView { // expected-error{{invalid use of borrow lifetime dependence for inout ownership}}
  return BufferView(x.ptr)
}

func invalidSpecifierPosition1(_ x: borrowing _borrow(x) BufferView) -> BufferView { // expected-error{{lifetime dependence specifiers may only be used on result of functions, methods, initializers}}
  return BufferView(x.ptr)
}

func invalidSpecifierPosition2(_ x: borrowing BufferView) -> BufferView { 
  let y: _borrow(x) x // expected-error{{lifetime dependence specifiers may only be used on result of functions, methods, initializers}}
  return BufferView(y.ptr)
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

  borrowing func borrowingMethodInvalidLifetimeDependence1() -> _consume(self) BufferView { // expected-error{{invalid use of consume lifetime dependence for borrowing ownership}}
    return view
  }

  borrowing func borrowingMethodInvalidLifetimeDependence2() -> _mutate(self) BufferView { // expected-error{{invalid use of mutate lifetime dependence for borrowing ownership}}
    return view
  }

  consuming func consumingMethodInvalidLifetimeDependence1() -> _borrow(self) BufferView { // expected-error{{invalid use of borrow lifetime dependence for consuming ownership}}
    return view
  }

  consuming func consumingMethodInvalidLifetimeDependence2() -> _mutate(self) BufferView { // expected-error{{invalid use of mutate lifetime dependence for consuming ownership}}
    return view
  }

  mutating func mutatingMethodInvalidLifetimeDependence1() -> _borrow(self) BufferView { // expected-error{{invalid use of borrow lifetime dependence for inout ownership}}
    return view
  }

  mutating func mutatingMethodInvalidLifetimeDependence2() -> _consume(self) BufferView { // expected-error{{invalid use of consume lifetime dependence for inout ownership}}
    return view
  } 
}

struct ArrayOfBufferView : ~Escapable {
    let arr: [BufferView]
    subscript(index: Int) -> _borrow(self) BufferView {
        return arr[index]
    }
}
