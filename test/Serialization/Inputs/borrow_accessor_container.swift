public struct Container<Element: ~Copyable >: ~Copyable {
  var _storage: UnsafeMutableBufferPointer<Element>
  public let _count: Int

  public init(_ storage: UnsafeMutableBufferPointer<Element>, _ count: Int) {
    self._storage = storage
    self._count = count
  }

  public var count: Int {
    return _count
  }

  public var first: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      return &_storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}

