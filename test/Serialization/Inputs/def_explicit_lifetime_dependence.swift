public struct AnotherView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @lifetime(borrow ptr)
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

public struct BufferView : ~Escapable {
  public let ptr: UnsafeRawBufferPointer
  @inlinable
  @lifetime(borrow ptr)
  public init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  @lifetime(borrow a)
  public init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
  }
  @lifetime(copy a)
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) {
    self.ptr = ptr
  }
  @lifetime(copy a, borrow b)
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView, _ b: borrowing Array<Int>) {
    self.ptr = ptr
  }
}

public struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  @lifetime(borrow ptr)
  public init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
}

@inlinable
@lifetime(borrow x)
public func derive(_ x: borrowing BufferView) -> BufferView {
  return BufferView(x.ptr)
}

public func use(_ x: borrowing BufferView) {}

@lifetime(borrow view)
public func borrowAndCreate(_ view: borrowing BufferView) -> BufferView {
  return BufferView(view.ptr)
}

@lifetime(copy view)
public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  return BufferView(view.ptr)
}

@lifetime(borrow this, copy that)
public func deriveThisOrThat(_ this: borrowing BufferView, _ that: borrowing BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this.ptr)
  }
  return BufferView(that.ptr)
}

public struct Wrapper : ~Escapable {
  var _view: BufferView
  @lifetime(copy view)
  public init(_ view: consuming BufferView) {
    self._view = view
  }
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
}

public enum FakeOptional<Wrapped: ~Escapable>: ~Escapable {
  case none, some(Wrapped)
}

extension FakeOptional: Escapable where Wrapped: Escapable {}

extension FakeOptional where Wrapped: ~Escapable {
  @lifetime(immortal)
  public init(_ nilLiteral: ()) {
    self = .none
  }
}

