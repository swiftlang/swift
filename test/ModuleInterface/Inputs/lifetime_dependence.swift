public struct AnotherView : ~Escapable {
  @usableFromInline let _ptr: UnsafeRawBufferPointer
  @usableFromInline let _count: Int
  @lifetime(borrow ptr)
  internal init(_ ptr: UnsafeRawBufferPointer, _ count: Int) {
    self._ptr = ptr
    self._count = count
  }
}

public struct BufferView : ~Escapable {
  @usableFromInline let _ptr: UnsafeRawBufferPointer
  @usableFromInline let _count: Int
  @usableFromInline 
  @lifetime(borrow ptr)
  internal init(_ ptr: UnsafeRawBufferPointer, _ count: Int) {
    self._ptr = ptr
    self._count = count
  }

  @inlinable
  @lifetime(borrow a)
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.init(ptr, a.count)
  }
  @inlinable
  @lifetime(a)
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) {
    self.init(ptr, a._count)
  }
}

@inlinable
@lifetime(x)
public func derive(_ x: consuming BufferView) -> BufferView {
  return BufferView(x._ptr, x._count)
}

@inlinable
public func use(_ x: consuming BufferView) {}

@inlinable
@lifetime(view)
public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  return BufferView(view._ptr, view._count)
}

@inlinable
@lifetime(this, that)
public func deriveThisOrThat(_ this: consuming BufferView, _ that: consuming BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this._ptr, this._count)
  }
  return BufferView(that._ptr, that._count)
}

