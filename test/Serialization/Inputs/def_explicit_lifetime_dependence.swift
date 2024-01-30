public struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

public struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

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

