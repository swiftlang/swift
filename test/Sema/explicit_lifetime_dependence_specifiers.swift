// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip   -enable-experimental-feature NoncopyableGenerics -enable-builtin-module -enable-experimental-feature BitwiseCopyable
// REQUIRES: asserts
// REQUIRES: noncopyable_generics
import Builtin

struct Container {
  let ptr: UnsafeRawBufferPointer
}

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  init(_ c: borrowing Container) -> _borrow(c) Self { // expected-error{{invalid lifetime dependence on bitwise copyable type}}
    self.ptr = c.ptr
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

func invalidTupleLifetimeDependence(_ x: inout BufferView) -> (_mutate(x) BufferView, BufferView) { // expected-error{{lifetime dependence specifiers cannot be applied to tuple elements}}
  return (BufferView(x.ptr), BufferView(x.ptr))
}

struct Wrapper : ~Escapable {
  let view: BufferView
  init(_ view: consuming BufferView) {
    self.view = view
  }
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

public struct GenericBufferView<Element> : ~Escapable {
  public typealias Index = Int
  public typealias Pointer = UnsafePointer<Element>

  public let baseAddress: Pointer
  public let count: Int

  public init<Storage>(unsafeBuffer: UnsafeBufferPointer<Element>,
                       storage: borrowing Storage)
    -> _borrow(storage) Self {
    let baseAddress = unsafeBuffer.baseAddress!
    self = GenericBufferView<Element>(baseAddress: baseAddress,
                                      count: unsafeBuffer.count)
    return self
  }
  // unsafe private API
  @_unsafeNonescapableResult
  init(baseAddress: Pointer, count: Int) {
    precondition(count >= 0, "Count must not be negative")
    self.baseAddress = baseAddress
    self.count = count
  } 
}
