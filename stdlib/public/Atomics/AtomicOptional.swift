//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol _PrimitiveAtomicOptional: _PrimitiveAtomic {
  var _isNil: Bool { get }
  static var _nilValue: Self { get }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Optional: AtomicValue
where Wrapped: AtomicValue, Wrapped._AtomicStorage: _PrimitiveAtomicOptional
{
  public typealias _AtomicStorage = Wrapped._AtomicStorage

  @_transparent @_alwaysEmitIntoClient
  public static func _prepareAtomicStorage(
    for value: __owned Self
  ) -> _AtomicStorage {
    guard let value = value else { return _AtomicStorage._nilValue }
    return Wrapped._prepareAtomicStorage(for: value)
  }

  @_transparent @_alwaysEmitIntoClient
  public static func _disposeAtomicStorage(
    _ storage: inout _AtomicStorage
  ) -> Self {
    guard !storage._isNil else { return nil }
    return Wrapped._disposeAtomicStorage(&storage)
  }

  @_transparent @_alwaysEmitIntoClient
  public static func _encodeAtomicStorage(
    for value: __owned Self
  ) -> _AtomicStorage {
    guard let value = value else { return _AtomicStorage._nilValue }
    return Wrapped._encodeAtomicStorage(for: value)
  }

  @_transparent @_alwaysEmitIntoClient
  public static func _decodeAtomicStorage(
    _ storage: __owned _AtomicStorage
  ) -> Self {
    guard !storage._isNil else { return nil }
    return Wrapped._decodeAtomicStorage(storage)
  }
}
