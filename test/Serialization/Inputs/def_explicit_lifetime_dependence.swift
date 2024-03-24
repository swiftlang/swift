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
  public init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) -> dependsOn(a) Self {
    self.ptr = ptr
    return self
  }
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) -> dependsOn(a) Self {
    self.ptr = ptr
    return self
  }
  public init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView, _ b: borrowing Array<Int>) -> dependsOn(a) dependsOn(b) Self {
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
public func derive(_ x: borrowing BufferView) -> dependsOn(scoped x) BufferView {
  return BufferView(x.ptr)
}

public func use(_ x: borrowing BufferView) {}

public func borrowAndCreate(_ view: borrowing BufferView) -> dependsOn(scoped view) BufferView {
  return BufferView(view.ptr)
}

public func consumeAndCreate(_ view: consuming BufferView) -> dependsOn(view) BufferView {
  return BufferView(view.ptr)
}

public func deriveThisOrThat(_ this: borrowing BufferView, _ that: borrowing BufferView) -> dependsOn(scoped this, that) BufferView {
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
