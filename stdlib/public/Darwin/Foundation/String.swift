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
// New Strings
//===----------------------------------------------------------------------===//

//
// Conversion from NSString to Swift's native representation
//

extension String {
  public init(_ cocoaString: NSString) {
    self = String(_cocoaString: cocoaString)
  }
}

@_effects(readonly)
private func _getClass(_ obj: AnyObject) -> AnyClass {
  return object_getClass(obj)!
}

@_effects(readonly)
private func _length(_ obj: AnyObject) -> Int {
  return CFStringGetLength(unsafeBitCast(obj, to: CFString.self))
}

@_effects(releasenone)
private func _copyString(_ obj: AnyObject) -> AnyObject {
  return CFStringCreateCopy(kCFAllocatorSystemDefault, unsafeBitCast(obj, to: CFString.self))
}

private let (nscfClass, nscfConstantClass): (AnyClass, AnyClass) =
  (objc_lookUpClass("__NSCFString")!,
   objc_lookUpClass("__NSCFConstantString")!)

extension String : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSString {
    // This method should not do anything extra except calling into the
    // implementation inside core.  (These two entry points should be
    // equivalent.)
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject, to: NSString.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) {
    result = String._unconditionallyBridgeFromObjectiveC(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> String {
    if let source = source {
      #if !(arch(i386) || arch(arm))
      if let result = _bridgeTaggedCocoaString(source) {
        return result
      }
      #endif
      
      let sourceClass:AnyClass = _getClass(source)
      
      if _isRebridgedSwiftStringClass(sourceClass) {
        return _rebridgeSwiftString(source, sourceClass)
      }
      
      let len = _length(source)

      //We *may* not be able to form a SmallString from this, but above 15 we
      //definitely can't. It's worth trying because eager bridging is almost
      //always good if we don't have to allocate for it.
      if len <= 15 && sourceClass == nscfClass,
        let result = _bridgeShortCFString(source, len) {
        return result
      }
      
      if sourceClass == nscfConstantClass {
        return _bridgeConstantCocoaString(source, len)
      }
      
      let immutableCopy = _copyString(source)
      
      //mutable->immutable might make it start being a tagged pointer
      #if !(arch(i386) || arch(arm))
      if let result = _bridgeTaggedCocoaString(immutableCopy) {
        return result
      }
      #endif
      
      return _bridgeUnknownCocoaString(immutableCopy, len)
    }
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    return String()
  }
}

extension Substring : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSString {
    return String(self)._bridgeToObjectiveC()
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSString,
    result: inout Substring?
  ) {
    let s = String(x)
    result = s[...]
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSString,
    result: inout Substring?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return result != nil
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> Substring {
    let str = String._unconditionallyBridgeFromObjectiveC(source)
    return str[...]
  }
}

extension String: CVarArg {}
