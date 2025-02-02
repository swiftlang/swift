@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
internal func _overrideLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  _ dependent: consuming T, borrowing source: borrowing U) -> T {
  dependent
}

@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(source)
internal func _overrideLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  _ dependent: consuming T, copying source: borrowing U) -> T {
  dependent
}

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
    let bv = BufferView(ptr, a.count)
    self = _overrideLifetime(bv, borrowing: a)
  }
  @inlinable
  @lifetime(a)
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) {
    let bv = BufferView(ptr, a._count)
    self = _overrideLifetime(bv, copying: a)
  }
}

@inlinable
@lifetime(x)
public func derive(_ x: consuming BufferView) -> BufferView {
  let pointer = x._ptr
  let bv = BufferView(pointer, x._count)
  return _overrideLifetime(bv, copying: x)
}

@inlinable
public func use(_ x: consuming BufferView) {}

@inlinable
@lifetime(view)
public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  let pointer = view._ptr
  let bv = BufferView(pointer, view._count)
  return _overrideLifetime(bv, copying: view)
}

@inlinable
@lifetime(this, that)
public func deriveThisOrThat(_ this: consuming BufferView, _ that: consuming BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    return BufferView(this._ptr, this._count)
  }
  return BufferView(that._ptr, that._count)
}

public struct Container {
  var buffer: UnsafeRawBufferPointer
  var object: AnyObject
}

extension Container {
  public var storage: BufferView {
    get {
      let view = BufferView(buffer, 1)
      return _overrideLifetime(view, borrowing: self)
    }
  }
}
