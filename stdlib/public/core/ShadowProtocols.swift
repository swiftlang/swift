//===--- ShadowProtocols.swift - Protocols for decoupled ObjC bridging ----===//
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
//  To implement bridging, the core standard library needs to interact
//  a little bit with Cocoa.  Because we want to keep the core
//  decoupled from the Foundation module, we can't use foundation
//  classes such as NSArray directly.  We _can_, however, use an @objc
//  protocols whose API is "layout-compatible" with that of NSArray,
//  and use unsafe casts to treat NSArray instances as instances of
//  that protocol.
//
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

@objc
public protocol _ShadowProtocol {}

/// A shadow for the `NSFastEnumeration` protocol.
@objc
public protocol _NSFastEnumeration : _ShadowProtocol {
  @objc(countByEnumeratingWithState:objects:count:)
  func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int
}

/// A shadow for the `NSEnumerator` class.
@objc
public protocol _NSEnumerator : _ShadowProtocol {
  init()
  func nextObject() -> AnyObject?
}

/// A token that can be used for `NSZone*`.
public typealias _SwiftNSZone = OpaquePointer

/// A shadow for the `NSCopying` protocol.
@objc
public protocol _NSCopying : _ShadowProtocol {
  @objc(copyWithZone:)
  func copy(with zone: _SwiftNSZone?) -> AnyObject
}

/// A shadow for the "core operations" of NSArray.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSArray` subclass.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSArrayCore :
    _NSCopying, _NSFastEnumeration {

  @objc(objectAtIndex:)
  func objectAt(_ index: Int) -> AnyObject

  func getObjects(_: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange)

  @objc(countByEnumeratingWithState:objects:count:)
  func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int

  var count: Int { get }
}

/// A shadow for the "core operations" of NSDictionary.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSDictionary` subclass.
@objc
public protocol _NSDictionaryCore :
    _NSCopying, _NSFastEnumeration {

  // The following methods should be overridden when implementing an
  // NSDictionary subclass.

  // The designated initializer of `NSDictionary`.
  init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer, count: Int)

  var count: Int { get }

  @objc(objectForKey:)
  func objectFor(_ aKey: AnyObject) -> AnyObject?

  func keyEnumerator() -> _NSEnumerator

  // We also override the following methods for efficiency.

  @objc(copyWithZone:)
  func copy(with zone: _SwiftNSZone?) -> AnyObject

  func getObjects(_ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?)

  @objc(countByEnumeratingWithState:objects:count:)
  func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int
}

/// A shadow for the API of `NSDictionary` we will use in the core
/// stdlib.
///
/// `NSDictionary` operations, in addition to those on
/// `_NSDictionaryCore`, that we need to use from the core stdlib.
/// Distinct from `_NSDictionaryCore` because we don't want to be
/// forced to implement operations that `NSDictionary` already
/// supplies.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSDictionary : _NSDictionaryCore {
  // Note! This API's type is different from what is imported by the clang
  // importer.
  func getObjects(_ objects: UnsafeMutablePointer<AnyObject>?,
      andKeys keys: UnsafeMutablePointer<AnyObject>?)
    }

/// A shadow for the "core operations" of NSSet.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSSet` subclass.
@objc
public protocol _NSSetCore :
    _NSCopying, _NSFastEnumeration {

  // The following methods should be overridden when implementing an
  // NSSet subclass.

  // The designated initializer of `NSSet`.
  init(objects: UnsafePointer<AnyObject?>, count: Int)

  var count: Int { get }
  func member(_ object: AnyObject) -> AnyObject?
  func objectEnumerator() -> _NSEnumerator

  // We also override the following methods for efficiency.

  @objc(copyWithZone:)
  func copy(with zone: _SwiftNSZone?) -> AnyObject

  @objc(countByEnumeratingWithState:objects:count:)
  func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int
}

/// A shadow for the API of NSSet we will use in the core
/// stdlib.
///
/// `NSSet` operations, in addition to those on
/// `_NSSetCore`, that we need to use from the core stdlib.
/// Distinct from `_NSSetCore` because we don't want to be
/// forced to implement operations that `NSSet` already
/// supplies.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSSet : _NSSetCore {
}

/// A shadow for the API of NSNumber we will use in the core
/// stdlib.
@objc
public protocol _NSNumber {
  var doubleValue: Double { get }
  var floatValue: Float { get }
  var unsignedLongLongValue: UInt64 { get }
  var longLongValue: Int64 { get }
  var objCType: UnsafePointer<Int8> { get }
}

#else

public protocol _NSArrayCore {}
public protocol _NSDictionaryCore {}
public protocol _NSSetCore {}

#endif
