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
import _SwiftFoundationOverlayShims

//===----------------------------------------------------------------------===//
// NSCoder
//===----------------------------------------------------------------------===//

@available(macOS 10.11, iOS 9.0, *)
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
    let result = __NSCoderDecodeObjectOfClassForKey(self, cls, key, nil)
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
    return __NSCoderDecodeObjectOfClassesForKey(self, classesAsNSObjects, key, nil).map { $0 as Any }
  }

  @nonobjc
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelObject() throws -> Any? {
    var error: NSError?
    let result = __NSCoderDecodeObject(self, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }

  @available(*, unavailable, renamed: "decodeTopLevelObject(forKey:)")
  public func decodeTopLevelObjectForKey(_ key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(swift, obsoleted: 4)
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(forKey key: String) throws -> AnyObject? {
    var error: NSError?
    let result = __NSCoderDecodeObjectForKey(self, key, &error)
    try resolveError(error)
    return result as AnyObject?
  }

  @nonobjc
  @available(swift, introduced: 4)
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(forKey key: String) throws -> Any? {
    var error: NSError?
    let result = __NSCoderDecodeObjectForKey(self, key, &error)
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

  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelObject<DecodedObjectType>(
    of cls: DecodedObjectType.Type, forKey key: String
  ) throws -> DecodedObjectType?
    where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    var error: NSError?
    let result = __NSCoderDecodeObjectOfClassForKey(self, cls, key, &error)
    try resolveError(error)
    return result as? DecodedObjectType
  }

  @nonobjc
  @available(*, unavailable, renamed: "decodeTopLevelObject(of:forKey:)")
  public func decodeTopLevelObjectOfClasses(_ classes: NSSet?, forKey key: String) throws -> AnyObject? {
    fatalError("This API has been renamed")
  }

  @nonobjc
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelObject(of classes: [AnyClass]?, forKey key: String) throws -> Any? {
    var error: NSError?
    var classesAsNSObjects: NSSet?
    if let theClasses = classes {
      classesAsNSObjects = NSSet(array: theClasses.map { $0 as AnyObject })
    }
    let result = __NSCoderDecodeObjectOfClassesForKey(self, classesAsNSObjects, key, &error)
    try resolveError(error)
    return result.map { $0 as Any }
  }
}

//===----------------------------------------------------------------------===//
// NSKeyedArchiver
//===----------------------------------------------------------------------===//

extension NSKeyedArchiver {
  @nonobjc
  @available(macOS 10.11, iOS 9.0, *)
  public func encodeEncodable<T : Encodable>(_ value: T, forKey key: String) throws {
    let plistEncoder = PropertyListEncoder()
    let plist = try plistEncoder.encodeToTopLevelContainer(value)
    self.encode(plist, forKey: key)
  }
}

//===----------------------------------------------------------------------===//
// NSKeyedUnarchiver
//===----------------------------------------------------------------------===//

extension NSKeyedUnarchiver {
  @nonobjc
  @available(swift, obsoleted: 4)
  @available(macOS 10.11, iOS 9.0, *)
  public class func unarchiveTopLevelObjectWithData(_ data: NSData) throws -> AnyObject? {
    var error: NSError?
    let result = __NSKeyedUnarchiverUnarchiveObject(self, data, &error)
    try resolveError(error)
    return result as AnyObject?
  }

  @nonobjc
  @available(swift, introduced: 4)
  @available(macOS 10.11, iOS 9.0, *)
  public class func unarchiveTopLevelObjectWithData(_ data: Data) throws -> Any? {
    var error: NSError?
    let result = __NSKeyedUnarchiverUnarchiveObject(self, data as NSData, &error)
    try resolveError(error)
    return result
  }

  @nonobjc
  @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
  public static func unarchivedObject<DecodedObjectType>(ofClass cls: DecodedObjectType.Type, from data: Data) throws -> DecodedObjectType? where DecodedObjectType : NSCoding, DecodedObjectType : NSObject {
    var error: NSError?
    let result = __NSKeyedUnarchiverSecureUnarchiveObjectOfClass(cls as AnyClass, data, &error)
    if let error = error { throw error }
    return result as? DecodedObjectType
  }

  @nonobjc
  @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
  public static func unarchivedObject(ofClasses classes: [AnyClass], from data: Data) throws -> Any? {
    var error: NSError?
    let classesAsNSObjects = NSSet(array: classes.map { $0 as AnyObject })
    let result = __NSKeyedUnarchiverSecureUnarchiveObjectOfClasses(classesAsNSObjects, data, &error)
    if let error = error { throw error }
    return result
  }
  
  @nonobjc
  private static let __plistClasses: [AnyClass] = [
    NSArray.self,
    NSData.self,
    NSDate.self,
    NSDictionary.self,
    NSNumber.self,
    NSString.self
  ]

  @nonobjc
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeDecodable<T : Decodable>(_ type: T.Type, forKey key: String) -> T? {
      guard let value = self.decodeObject(of: NSKeyedUnarchiver.__plistClasses, forKey: key) else {
          return nil
      }

      let plistDecoder = PropertyListDecoder()
      do {
          return try plistDecoder.decode(T.self, fromTopLevel: value)
      } catch {
          self.failWithError(error)
          return nil
      }
  }

  @nonobjc
  @available(macOS 10.11, iOS 9.0, *)
  public func decodeTopLevelDecodable<T : Decodable>(_ type: T.Type, forKey key: String) throws  -> T? {
    guard let value = try self.decodeTopLevelObject(of: NSKeyedUnarchiver.__plistClasses, forKey: key) else {
      return nil
    }

    let plistDecoder = PropertyListDecoder()
    do {
      return try plistDecoder.decode(T.self, fromTopLevel: value)
    } catch {
      self.failWithError(error)
      throw error;
    }
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
