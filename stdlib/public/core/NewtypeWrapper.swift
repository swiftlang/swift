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
public protocol _SwiftNewtypeWrapper : RawRepresentable { }

extension _SwiftNewtypeWrapper where Self: Hashable, Self.RawValue : Hashable {
  @inlinable // FIXME(sil-serialize-all)
  public var hashValue: Int {
    return rawValue.hashValue
  }

  @inlinable // FIXME(sil-serialize-all)
  public func hash(into hasher: inout Hasher) {
    hasher.combine(rawValue)
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
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: Self.RawValue?
  ) -> Self {
    return Self(rawValue: source!)!
  }
}
#endif

