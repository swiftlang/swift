//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import CoreFoundation
import CoreGraphics

//===----------------------------------------------------------------------===//
// NSObject
//===----------------------------------------------------------------------===//

// These conformances should be located in the `ObjectiveC` module, but they can't
// be placed there because string bridging is not available there.
extension NSObject : CustomStringConvertible {}
extension NSObject : CustomDebugStringConvertible {}

public let NSNotFound: Int = .max

//===----------------------------------------------------------------------===//
// NSLocalizedString
//===----------------------------------------------------------------------===//

/// Returns a localized string, using the main bundle if one is not specified.
public
func NSLocalizedString(_ key: String,
                       tableName: String? = nil,
                       bundle: Bundle = Bundle.main,
                       value: String = "",
                       comment: String) -> String {
  return bundle.localizedString(forKey: key, value:value, table:tableName)
}

//===----------------------------------------------------------------------===//
// NSLog
//===----------------------------------------------------------------------===//

public func NSLog(_ format: String, _ args: CVarArg...) {
  withVaList(args) { NSLogv(format, $0) }
}

//===----------------------------------------------------------------------===//
// NSCoder
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSCoder_decodeObject")
internal func NS_Swift_NSCoder_decodeObject(
  _ self_: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectForKey")
internal func NS_Swift_NSCoder_decodeObjectForKey(
  _ self_: AnyObject,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectOfClassForKey")
internal func NS_Swift_NSCoder_decodeObjectOfClassForKey(
  _ self_: AnyObject,
  _ cls: AnyObject,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

@_silgen_name("NS_Swift_NSCoder_decodeObjectOfClassesForKey")
internal func NS_Swift_NSCoder_decodeObjectOfClassesForKey(
  _ self_: AnyObject,
  _ classes: NSSet?,
  _ key: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?


@available(OSX 10.11, iOS 9.0, *)
internal func resolveError(_ error: NSError?) throws {
  if let error = error, error.code != NSCoderValueNotFoundError {
    throw error
  }
}

extension NSCoder {
  @available(*, unavailable, renamed: "decodeObject(of:forKey:)")
  public func decodeObjectOfClass<DecodedObjectType>(
    _ cls: DecodedObjectType.Type, forKey key: String
  ) -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    fatalError("This API has been renamed")
  }

  public func decodeObject<DecodedObjectType>(
    of cls: DecodedObjectType.Type, forKey key: String
  ) -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    let result = NS_Swift_NSCoder_decodeObjectOfClassForKey(self as AnyObject, cls as AnyObject, key as AnyObject, nil)
    return result as? DecodedObjectType
  }

  @available(*, unavailable, renamed: "decodeObject(of:forKey:)")
  @nonobjc
  public func decodeObjectOfClasses(_ classes: NSSet?, forKey key: String) -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  public func decodeObject(of classes: [AnyClass]?, forKey key: String) -> Any? {
    var classesAsNSObjects: NSSet?
    if let theClasses = classes {
      classesAsNSObjects = NSSet(array: theClasses.map { $0 as AnyObject })
    }
    return NS_Swift_NSCoder_decodeObjectOfClassesForKey(self as AnyObject, classesAsNSObjects, key as AnyObject, nil).map { $0 as Any }
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject() throws -> Any? {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObject(self as AnyObject, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }

  @available(*, unavailable, renamed: "decodeTopLevelObject(forKey:)")
  public func decodeTopLevelObjectForKey(_ key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(forKey key: String) throws -> AnyObject? {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObjectForKey(self as AnyObject, key as AnyObject, &error)
    try resolveError(error)
    return result
  }

  @available(*, unavailable, renamed: "decodeTopLevelObject(of:forKey:)")
  public func decodeTopLevelObjectOfClass<DecodedObjectType>(
    _ cls: DecodedObjectType.Type, forKey key: String
  ) throws -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    fatalError("This API has been renamed")
  }

  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject<DecodedObjectType>(
    of cls: DecodedObjectType.Type, forKey key: String
  ) throws -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    var error: NSError?
    let result = NS_Swift_NSCoder_decodeObjectOfClassForKey(self as AnyObject, cls as AnyObject, key as AnyObject, &error)
    try resolveError(error)
    return result as? DecodedObjectType
  }

  @nonobjc
  @available(*, unavailable, renamed: "decodeTopLevelObject(of:forKey:)")
  public func decodeTopLevelObjectOfClasses(_ classes: NSSet?, forKey key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(OSX 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(of classes: [AnyClass]?, forKey key: String) throws -> Any? {
    var error: NSError?
    var classesAsNSObjects: NSSet?
    if let theClasses = classes {
      classesAsNSObjects = NSSet(array: theClasses.map { $0 as AnyObject })
    }
    let result = NS_Swift_NSCoder_decodeObjectOfClassesForKey(self as AnyObject, classesAsNSObjects, key as AnyObject, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }
}

//===----------------------------------------------------------------------===//
// NSKeyedUnarchiver
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData")
internal func NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(
  _ self_: AnyObject,
  _ data: AnyObject,
  _ error: NSErrorPointer) -> AnyObject?

extension NSKeyedUnarchiver {
  @available(OSX 10.11, iOS 9.0, *)
  @nonobjc
  public class func unarchiveTopLevelObjectWithData(_ data: NSData) throws -> AnyObject? {
    var error: NSError?
    let result = NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(self, data as AnyObject, &error)
    try resolveError(error)
    return result
  }
}

//===----------------------------------------------------------------------===//
// Mirror/Quick Look Conformance
//===----------------------------------------------------------------------===//

extension NSURL : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    guard let str = absoluteString else { return .text("Unknown URL") }
    return .url(str)
  }
}

extension NSDate : CustomPlaygroundQuickLookable {
  var summary: String {
    let df = DateFormatter()
    df.dateStyle = .medium
    df.timeStyle = .short
    return df.string(from: self as Date)
  }

  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(summary)
  }
}

@available(*, deprecated, renamed:"NSCoding", message: "Please use NSCoding")
typealias Coding = NSCoding

@available(*, deprecated, renamed:"NSCoder", message: "Please use NSCoder")
typealias Coder = NSCoder

@available(*, deprecated, renamed:"NSKeyedUnarchiver", message: "Please use NSKeyedUnarchiver")
typealias KeyedUnarchiver = NSKeyedUnarchiver

@available(*, deprecated, renamed:"NSKeyedArchiver", message: "Please use NSKeyedArchiver")
typealias KeyedArchiver = NSKeyedArchiver

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//

extension AnyHashable : _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSObject {
    // This is unprincipled, but pretty much any object we'll encounter in
    // Swift is NSObject-conforming enough to have -hash and -isEqual:.
    return unsafeBitCast(base as AnyObject, to: NSObject.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) {
    result = AnyHashable(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSObject,
    result: inout AnyHashable?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSObject?
  ) -> AnyHashable {
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    if _slowPath(source == nil) { return AnyHashable(String()) }
    return AnyHashable(source!)
  }
}

//===----------------------------------------------------------------------===//
// CVarArg for bridged types
//===----------------------------------------------------------------------===//

extension CVarArg where Self: _ObjectiveCBridgeable {
  /// Default implementation for bridgeable types.
  public var _cVarArgEncoding: [Int] {
    let object = self._bridgeToObjectiveC()
    _autorelease(object)
    return _encodeBitsAsWords(object)
  }
}
