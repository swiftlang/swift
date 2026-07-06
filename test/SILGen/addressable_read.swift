// RUN: %target-swift-frontend -emit-sil -verify  -enable-experimental-feature AddressableParameters %s

// REQUIRES: swift_feature_AddressableParameters

public struct Container<Element: ~Copyable >: ~Copyable {
  var _storage: UnsafeMutableBufferPointer<Element>
  var _count: Int

  public subscript(index: Int) -> Element {
    @_addressableSelf
    _read {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      yield _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
    _modify {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      yield &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}

