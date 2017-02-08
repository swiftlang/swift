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

@_exported import Foundation // Clang module


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


@available(*, deprecated, renamed:"NSCoding", message: "Please use NSCoding")
typealias Coding = NSCoding

@available(*, deprecated, renamed:"NSCoder", message: "Please use NSCoder")
typealias Coder = NSCoder

@available(*, deprecated, renamed:"NSKeyedUnarchiver", message: "Please use NSKeyedUnarchiver")
typealias KeyedUnarchiver = NSKeyedUnarchiver

@available(*, deprecated, renamed:"NSKeyedArchiver", message: "Please use NSKeyedArchiver")
typealias KeyedArchiver = NSKeyedArchiver
