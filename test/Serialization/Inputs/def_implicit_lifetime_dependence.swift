public struct BufferView : ~Escapable {
  public let ptr: UnsafeRawBufferPointer
  public let c: Int
  @lifetime(borrow ptr)
  public init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  @inlinable
  @lifetime(copy otherBV)
  public init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
}

public struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
  @lifetime(borrow ptr)
  public init(_ ptr: UnsafeMutableRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

@inlinable
@lifetime(copy x)
public func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr, x.c)
}

@lifetime(copy view)
public func borrowAndCreate(_ view: borrowing BufferView) -> BufferView {
  return BufferView(view.ptr, view.c )
}

@lifetime(copy view)
public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  return BufferView(view.ptr, view.c)
}

public struct Container : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  public init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }

  public var view: BufferView {
    get {
      return BufferView(ptr, c)
    }
  }
}

public struct Wrapper : ~Escapable {
  var _view: BufferView
  public var view: BufferView {
    @lifetime(copy self)
    _read {
      yield _view
    }
    @lifetime(&self)
    _modify {
      yield &_view
    }
  }
  @lifetime(copy view)
  public init(_ view: consuming BufferView) {
    self._view = view
  }
}

