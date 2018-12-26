//===-- KeyPathIterable.swift ---------------------------------*- swift -*-===//
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
//
// This file defines the KeyPathIterable protocol.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// KeyPathIterable
//===----------------------------------------------------------------------===//

/// An implementation detail of `KeyPathIterable`; do not use this protocol
/// directly.
public protocol _KeyPathIterableBase {
  var _allKeyPathsTypeErased: [AnyKeyPath] { get }
  var _recursivelyAllKeyPathsTypeErased: [AnyKeyPath] { get }
}

/// A type whose values provides custom key paths to properties or elements.
public protocol KeyPathIterable : _KeyPathIterableBase {
  /// A type that can represent a collection of all key paths of this type.
  associatedtype AllKeyPaths : Collection
    where AllKeyPaths.Element == PartialKeyPath<Self>

  /// A collection of all custom key paths of this value.
  var allKeyPaths: AllKeyPaths { get }
}

public extension KeyPathIterable {
  /// An array of all custom key paths of this value and any custom key paths
  /// nested within each of what this value's key paths refers to.
  var recursivelyAllKeyPaths: [PartialKeyPath<Self>] {
    var result: [PartialKeyPath<Self>] = []
    for kp in allKeyPaths {
      result.append(kp)
      if let nested = self[keyPath: kp] as? _KeyPathIterableBase {
        for nkp in nested._recursivelyAllKeyPathsTypeErased {
          result.append(kp.appending(path: nkp)!)
        }
      }
    }
    return result
  }
}

extension KeyPathIterable {
  public var _allKeyPathsTypeErased: [AnyKeyPath] {
    return allKeyPaths.map { $0 as AnyKeyPath }
  }
  public var _recursivelyAllKeyPathsTypeErased: [AnyKeyPath] {
    return recursivelyAllKeyPaths.map { $0 as AnyKeyPath }
  }
}

public extension KeyPathIterable {
  /// Returns an array of all custom key paths of this value, to the specified
  /// type.
  @inlinable
  func allKeyPaths<T>(to _: T.Type) -> [KeyPath<Self, T>] {
    return allKeyPaths.compactMap { $0 as? KeyPath<Self, T> }
  }

  /// Returns an array of all custom key paths of this value and any custom key
  /// paths nested within each of what this value's key paths refers to, to
  /// the specified type.
  @inlinable
  func recursivelyAllKeyPaths<T>(to _: T.Type) -> [KeyPath<Self, T>] {
    return recursivelyAllKeyPaths.compactMap { $0 as? KeyPath<Self, T> }
  }

  /// Returns an array of all custom writable key paths of this value, to the
  /// specified type.
  func allWritableKeyPaths<T>(to _: T.Type) -> [WritableKeyPath<Self, T>] {
    return allKeyPaths(to: T.self)
      .compactMap { $0 as? WritableKeyPath<Self, T> }
  }

  /// Returns an array of all custom writable key paths of this value and any
  /// custom writable key paths nested within each of what this value's key
  /// paths refers to, to the specified type.
  @inlinable
  func recursivelyAllWritableKeyPaths<T>(
    to _: T.Type
  ) -> [WritableKeyPath<Self, T>] {
    return recursivelyAllKeyPaths(to: T.self)
      .compactMap { $0 as? WritableKeyPath<Self, T> }
  }
}

//===----------------------------------------------------------------------===//
// Collection conformances
//===----------------------------------------------------------------------===//

extension Array : KeyPathIterable {
  public typealias AllKeyPaths = [PartialKeyPath<Array>]
  public var allKeyPaths: [PartialKeyPath<Array>] {
    return indices.map { \Array[$0] }
  }
}

extension Dictionary : KeyPathIterable {
  public typealias AllKeyPaths = [PartialKeyPath<Dictionary>]
  public var allKeyPaths: [PartialKeyPath<Dictionary>] {
    return keys.map { \Dictionary[$0]! }
  }
}
