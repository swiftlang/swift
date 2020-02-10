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

extension String : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSString {
    // This method should not do anything extra except calling into the
    // implementation inside core.  (These two entry points should be
    // equivalent.)
    return unsafeBitCast(_bridgeToObjectiveCImpl(), to: NSString.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) {
    result = String(x)
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
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty string.
    if _slowPath(source == nil) { return String() }
    return String(source!)
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
    // `nil` has historically been used as a stand-in for an empty
    // string; map it to an empty substring.
    if _slowPath(source == nil) { return Substring() }
    let s = String(source!)
    return s[...]
  }
}

extension String: CVarArg {}

/*
 This is on NSObject so that the stdlib can call it in StringBridge.swift
 without having to synthesize a receiver (e.g. lookup a class or allocate)
 
 In the future (once the Foundation overlay can know about SmallString), we
 should move the caller of this method up into the overlay and avoid this
 indirection.
 */
private extension NSObject {
  // The ObjC selector has to start with "new" to get ARC to not autorelease
  @_effects(releasenone)
  @objc(newTaggedNSStringWithASCIIBytes_:length_:)
  func createTaggedString(bytes: UnsafePointer<UInt8>,
                          count: Int) -> AnyObject? {
    //TODO: update this to use _CFStringCreateTaggedPointerString once we can
    return CFStringCreateWithBytes(
      kCFAllocatorSystemDefault,
      bytes,
      count,
      CFStringBuiltInEncodings.UTF8.rawValue,
      false
    ) as NSString as NSString? //just "as AnyObject" inserts unwanted bridging
  }
}
