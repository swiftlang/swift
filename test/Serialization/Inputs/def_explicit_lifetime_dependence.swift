public struct AnotherView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

public struct BufferView : ~Escapable {
  public let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  @inlinable
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  public init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) -> _borrow(a) Self {
    self.ptr = ptr
    return self
  }
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) -> _consume(a) Self {
    self.ptr = ptr
    return self
  }
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView, _ b: borrowing Array<Int>) -> _consume(a) _borrow(b) Self {
    self.ptr = ptr
    return self
  }
}

public struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

@inlinable
public func derive(_ x: borrowing BufferView) -> _borrow(x) BufferView {
  return BufferView(x.ptr)
}

public func use(_ x: borrowing BufferView) {}

public func borrowAndCreate(_ view: borrowing BufferView) -> _borrow(view) BufferView {
  return BufferView(view.ptr)
}

public func consumeAndCreate(_ view: consuming BufferView) -> _consume(view) BufferView {
  return BufferView(view.ptr)
}

public func deriveThisOrThat(_ this: borrowing BufferView, _ that: borrowing BufferView) -> _borrow(this, that) BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this.ptr)
  }
  return BufferView(that.ptr)
}

public struct Wrapper : ~Escapable {
  var _view: BufferView
  public init(_ view: consuming BufferView) {
    self._view = view
  }
  public var view: BufferView {
    _read {
      yield _view
    }
    _modify {
      yield &_view
    }
  }
}
