//===--- ShadowProtocols.swift - Protocols for decoupled ObjC bridging ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
public protocol _NSFastEnumerationType : _ShadowProtocol {
  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int
}

/// A shadow for the `NSEnumerator` class.
@objc
public protocol _NSEnumeratorType : _ShadowProtocol {
  init()
  func nextObject() -> AnyObject?
}

/// A token that can be used for `NSZone*`.
public typealias _SwiftNSZone = COpaquePointer

/// A shadow for the `NSCopying` protocol.
@objc
public protocol _NSCopyingType : _ShadowProtocol {
  func copyWithZone(zone: _SwiftNSZone) -> AnyObject
}

/// A shadow for the "core operations" of NSArray.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSArray` subclass.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSArrayCoreType :
    _NSCopyingType, _NSFastEnumerationType {

  func objectAtIndex(index: Int) -> AnyObject

  func getObjects(UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange)

  func countByEnumeratingWithState(
         state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
         objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int

  var count: Int { get }
}

/// A shadow for the "core operations" of NSDictionary.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSDictionary` subclass.
@objc
public protocol _NSDictionaryCoreType :
    _NSCopyingType, _NSFastEnumerationType {

  // The following methods should be overridden when implementing an
  // NSDictionary subclass.

  // The designated initializer of `NSDictionary`.
  init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafePointer<Void>, count: Int)

  var count: Int { get }
  func objectForKey(aKey: AnyObject) -> AnyObject?
  func keyEnumerator() -> _NSEnumeratorType

  // We also override the following methods for efficiency.

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  func getObjects(objects: UnsafeMutablePointer<AnyObject>,
    andKeys keys: UnsafeMutablePointer<AnyObject>)

  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int
}

/// A shadow for the API of `NSDictionary` we will use in the core
/// stdlib.
///
/// `NSDictionary` operations, in addition to those on
/// `_NSDictionaryCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSDictionaryCoreType` because we don't want to be
/// forced to implement operations that `NSDictionary` already
/// supplies.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSDictionaryType : _NSDictionaryCoreType {
  // Note! This API's type is different from what is imported by the clang
  // importer.
  func getObjects(objects: UnsafeMutablePointer<AnyObject>,
      andKeys keys: UnsafeMutablePointer<AnyObject>)
}

/// A shadow for the "core operations" of NSSet.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSSet` subclass.
@objc
public protocol _NSSetCoreType :
    _NSCopyingType, _NSFastEnumerationType {

  // The following methods should be overridden when implementing an
  // NSSet subclass.

  // The designated initializer of `NSSet`.
  init(objects: UnsafePointer<AnyObject?>, count: Int)

  var count: Int { get }
  func member(object: AnyObject) -> AnyObject?
  func objectEnumerator() -> _NSEnumeratorType

  // We also override the following methods for efficiency.

  func copyWithZone(zone: _SwiftNSZone) -> AnyObject

  func countByEnumeratingWithState(
    state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>, count: Int
  ) -> Int
}

/// A shadow for the API of NSSet we will use in the core
/// stdlib.
///
/// `NSSet` operations, in addition to those on
/// `_NSSetCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSSetCoreType` because we don't want to be
/// forced to implement operations that `NSSet` already
/// supplies.
@unsafe_no_objc_tagged_pointer @objc
public protocol _NSSetType : _NSSetCoreType {
}

#endif
