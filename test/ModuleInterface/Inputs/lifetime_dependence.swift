public struct AnotherView : ~Escapable {
  @usableFromInline let _ptr: UnsafeRawBufferPointer
  @usableFromInline let _count: Int
  internal init(_ ptr: UnsafeRawBufferPointer, _ count: Int) -> dependsOn(ptr) Self {
    self._ptr = ptr
    self._count = count
  }
}

public struct BufferView : ~Escapable {
  @usableFromInline let _ptr: UnsafeRawBufferPointer
  @usableFromInline let _count: Int
  @usableFromInline 
  internal init(_ ptr: UnsafeRawBufferPointer, _ count: Int) -> dependsOn(ptr) Self  {
    self._ptr = ptr
    self._count = count
  }

  @inlinable
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) -> dependsOn(a) Self {
    self.init(ptr, a.count)
    return self
  }
  @inlinable
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) -> dependsOn(a) Self {
    self.init(ptr, a._count)
    return self
  }
}

@inlinable
public func derive(_ x: consuming BufferView) -> dependsOn(x) BufferView {
  return BufferView(x._ptr, x._count)
}

@inlinable
public func use(_ x: consuming BufferView) {}

@inlinable
public func consumeAndCreate(_ view: consuming BufferView) -> dependsOn(view) BufferView {
  return BufferView(view._ptr, view._count)
}

@inlinable
public func deriveThisOrThat(_ this: consuming BufferView, _ that: consuming BufferView) -> dependsOn(this, that) BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this._ptr, this._count)
  }
  return BufferView(that._ptr, that._count)
}

