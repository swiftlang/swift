//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An implementation detail used to implement support importing
/// (Objective-)C entities marked with the swift_newtype Clang
/// attribute.
public protocol _SwiftNewtypeWrapper
: RawRepresentable, _HasCustomAnyHashableRepresentation { }

extension _SwiftNewtypeWrapper where Self: Hashable, Self.RawValue: Hashable {
  /// The hash value.
  @inlinable
  public var hashValue: Int {
    return rawValue.hashValue
  }

  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(rawValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func _rawHashValue(seed: Int) -> Int {
    return rawValue._rawHashValue(seed: seed)
  }
}

extension _SwiftNewtypeWrapper {
  public __consuming func _toCustomAnyHashable() -> AnyHashable? {
    return nil
  }
}

extension _SwiftNewtypeWrapper where Self: Hashable, Self.RawValue: Hashable {
  public __consuming func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _NewtypeWrapperAnyHashableBox(self))
  }
}

internal struct _NewtypeWrapperAnyHashableBox<Base>: _AnyHashableBox
where Base: _SwiftNewtypeWrapper & Hashable, Base.RawValue: Hashable {
  var _value: Base

  init(_ value: Base) {
    self._value = value
  }

  var _canonicalBox: _AnyHashableBox {
    return (_value.rawValue as AnyHashable)._box._canonicalBox
  }

  func _isEqual(to other: _AnyHashableBox) -> Bool? {
    _preconditionFailure("_isEqual called on non-canonical AnyHashable box")
  }

  var _hashValue: Int {
    _preconditionFailure("_hashValue called on non-canonical AnyHashable box")
  }

  func _hash(into hasher: inout Hasher) {
    _preconditionFailure("_hash(into:) called on non-canonical AnyHashable box")
  }

  func _rawHashValue(_seed: Int) -> Int {
    _preconditionFailure("_rawHashValue(_seed:) called on non-canonical AnyHashable box")
  }

  var _base: Any { return _value }

  func _unbox<T: Hashable>() -> T? {
    return _value as? T ?? _value.rawValue as? T
  }

  func _downCastConditional<T>(into result: UnsafeMutablePointer<T>) -> Bool {
    if let value = _value as? T {
      result.initialize(to: value)
      return true
    }
    if let value = _value.rawValue as? T {
      result.initialize(to: value)
      return true
    }
    return false
  }
}

#if _runtime(_ObjC)
extension _SwiftNewtypeWrapper where Self.RawValue : _ObjectiveCBridgeable {
  // Note: This is the only default typealias for _ObjectiveCType, because
  // constrained extensions aren't allowed to define types in different ways.
  // Fortunately the others don't need it.
  public typealias _ObjectiveCType = Self.RawValue._ObjectiveCType

  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveC() -> Self.RawValue._ObjectiveCType {
    return rawValue._bridgeToObjectiveC()
  }
  @inlinable // FIXME(sil-serialize-all)
  public static func _forceBridgeFromObjectiveC(
    _ source: Self.RawValue._ObjectiveCType,
    result: inout Self?
  ) {
    var innerResult: Self.RawValue?
    Self.RawValue._forceBridgeFromObjectiveC(source, result: &innerResult)
    result = innerResult.flatMap { Self(rawValue: $0) }
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _conditionallyBridgeFromObjectiveC(
    _ source: Self.RawValue._ObjectiveCType,
    result: inout Self?
  ) -> Bool {
    var innerResult: Self.RawValue?
    let success = Self.RawValue._conditionallyBridgeFromObjectiveC(
      source,
      result: &innerResult)
    result = innerResult.flatMap { Self(rawValue: $0) }
    return success
  }

  @inlinable // FIXME(sil-serialize-all)
  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: Self.RawValue._ObjectiveCType?
  ) -> Self {
    return Self(
      rawValue: Self.RawValue._unconditionallyBridgeFromObjectiveC(source))!
  }
}

extension _SwiftNewtypeWrapper where Self.RawValue: AnyObject {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveC() -> Self.RawValue {
    return rawValue
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _forceBridgeFromObjectiveC(
    _ source: Self.RawValue,
    result: inout Self?
  ) {
    result = Self(rawValue: source)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func _conditionallyBridgeFromObjectiveC(
    _ source: Self.RawValue,
    result: inout Self?
  ) -> Bool {
    result = Self(rawValue: source)
    return result != nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: Self.RawValue?
  ) -> Self {
    return Self(rawValue: source!)!
  }
}
#endif

