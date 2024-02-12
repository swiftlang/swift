public struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  public init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
  }
}

public struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @_unsafeNonescapableResult
  public init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

public func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr)
}

public func use(_ x: borrowing BufferView) {}

public func borrowAndCreate(_ view: borrowing BufferView) -> BufferView {
  return BufferView(view.ptr)
}

public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  return BufferView(view.ptr)
}

