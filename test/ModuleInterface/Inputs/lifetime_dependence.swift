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
@lifetime(copy source)
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
  @lifetime(copy a)
  internal init(_ ptr: UnsafeRawBufferPointer, _ a: consuming AnotherView) {
    let bv = BufferView(ptr, a._count)
    self = _overrideLifetime(bv, copying: a)
  }
}

@inlinable
@lifetime(copy x)
public func derive(_ x: consuming BufferView) -> BufferView {
  let pointer = x._ptr
  let bv = BufferView(pointer, x._count)
  return _overrideLifetime(bv, copying: x)
}

@inlinable
public func use(_ x: consuming BufferView) {}

@inlinable
@lifetime(copy view)
public func consumeAndCreate(_ view: consuming BufferView) -> BufferView {
  let pointer = view._ptr
  let bv = BufferView(pointer, view._count)
  return _overrideLifetime(bv, copying: view)
}

// FIXME: Filed rdar://150398673 ([nonescapable] allocbox-to-stack fails causing lifetime diagnostics to fail)
// Remove _overrideLifetime when this is fixed.
@inlinable
@lifetime(copy this, copy that)
public func deriveThisOrThat(_ this: consuming BufferView, _ that: consuming BufferView) -> BufferView {
  if (Int.random(in: 1..<100) == 0) {
    let thisView = BufferView(this._ptr, this._count)
    return _overrideLifetime(thisView, copying: this)
  }
  let thatView = BufferView(that._ptr, that._count)
  return _overrideLifetime(thatView, copying: that)
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
