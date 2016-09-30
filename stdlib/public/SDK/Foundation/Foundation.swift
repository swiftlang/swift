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

// FIXME: move inside NSIndexSet when the compiler supports this.
public struct NSIndexSetIterator : IteratorProtocol {
  public typealias Element = Int

  internal let _set: NSIndexSet
  internal var _first: Bool = true
  internal var _current: Int?

  internal init(set: NSIndexSet) {
    self._set = set
    self._current = nil
  }

  public mutating func next() -> Int? {
    if _first {
      _current = _set.firstIndex
      _first = false
    } else if let c = _current {
      _current = _set.indexGreaterThanIndex(c)
    } else {
      // current is already nil
    }
    if _current == NSNotFound {
      _current = nil
    }
    return _current
  }
}

extension NSIndexSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSIndexSetIterator {
    return NSIndexSetIterator(set: self)
  }
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension NSRange {
  public init(_ x: Range<Int>) {
    location = x.lowerBound
    length = x.count
  }

  // FIXME(ABI)#75 (Conditional Conformance): this API should be an extension on Range.
  // Can't express it now because the compiler does not support conditional
  // extensions with type equality constraints.
  public func toRange() -> Range<Int>? {
    if location == NSNotFound { return nil }
    return location..<(location+length)
  }
}

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

#if os(OSX)

//===----------------------------------------------------------------------===//
// NSRectEdge
//===----------------------------------------------------------------------===//

// In the SDK, the following NS*Edge constants are defined as macros for the
// corresponding CGRectEdge enumerators.  Thus, in the SDK, NS*Edge constants
// have CGRectEdge type.  This is not correct for Swift (as there is no
// implicit conversion to NSRectEdge).

@available(*, unavailable, renamed: "NSRectEdge.MinX")
public var NSMinXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MinY")
public var NSMinYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxX")
public var NSMaxXEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}
@available(*, unavailable, renamed: "NSRectEdge.MaxY")
public var NSMaxYEdge: NSRectEdge {
  fatalError("unavailable property can't be accessed")
}

extension NSRectEdge {
  public init(rectEdge: CGRectEdge) {
    self = NSRectEdge(rawValue: UInt(rectEdge.rawValue))!
  }
}

extension CGRectEdge {
  public init(rectEdge: NSRectEdge) {
    self = CGRectEdge(rawValue: UInt32(rectEdge.rawValue))!
  }
}

#endif

//===----------------------------------------------------------------------===//
// NSError (as an out parameter).
//===----------------------------------------------------------------------===//

public typealias NSErrorPointer = AutoreleasingUnsafeMutablePointer<NSError?>?

// Note: NSErrorPointer becomes ErrorPointer in Swift 3.
public typealias ErrorPointer = NSErrorPointer

public // COMPILER_INTRINSIC
let _nilObjCError: Error = _GenericObjCError.nilError

@_silgen_name("swift_convertNSErrorToError")
public // COMPILER_INTRINSIC
func _convertNSErrorToError(_ error: NSError?) -> Error {
  if let error = error {
    return error
  }
  return _nilObjCError
}

@_silgen_name("swift_convertErrorToNSError")
public // COMPILER_INTRINSIC
func _convertErrorToNSError(_ error: Error) -> NSError {
  return unsafeDowncast(_bridgeErrorToNSError(error), to: NSError.self)
}

//===----------------------------------------------------------------------===//
// Variadic initializers and methods
//===----------------------------------------------------------------------===//

extension NSPredicate {
  // + (NSPredicate *)predicateWithFormat:(NSString *)predicateFormat, ...;
  public
  convenience init(format predicateFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    self.init(format: predicateFormat, arguments: va_args)
  }
}

extension NSExpression {
  // + (NSExpression *) expressionWithFormat:(NSString *)expressionFormat, ...;
  public
  convenience init(format expressionFormat: String, _ args: CVarArg...) {
    let va_args = getVaList(args)
    self.init(format: expressionFormat, arguments: va_args)
  }
}

//===----------------------------------------------------------------------===//
// NSUndoManager
//===----------------------------------------------------------------------===//

@_silgen_name("NS_Swift_NSUndoManager_registerUndoWithTargetHandler")
internal func NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
  _ self_: AnyObject,
  _ target: AnyObject,
  _ handler: @escaping @convention(block) (AnyObject) -> Void)

extension UndoManager {
  @available(*, unavailable, renamed: "registerUndo(withTarget:handler:)")
  public func registerUndoWithTarget<TargetType : AnyObject>(_ target: TargetType, handler: (TargetType) -> Void) {
    fatalError("This API has been renamed")
  }

  @available(OSX 10.11, iOS 9.0, *)
  public func registerUndo<TargetType : AnyObject>(withTarget target: TargetType, handler: @escaping (TargetType) -> Void) {
    // The generic blocks use a different ABI, so we need to wrap the provided
    // handler in something ObjC compatible.
    let objcCompatibleHandler: (AnyObject) -> Void = { internalTarget in
      handler(internalTarget as! TargetType)
    }
    NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
      self as AnyObject, target as AnyObject, objcCompatibleHandler)
  }
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

extension NSRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["location": location, "length": length])
  }
}

extension NSRange : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .range(Int64(location), Int64(length))
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
