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
    self = String._unconditionallyBridgeFromObjectiveC(cocoaString)
  }
}

@_effects(readonly)
private func _getClass(_ obj: NSString) -> AnyClass {
  return object_getClass(obj)!
}

@_effects(readonly)
private func _length(_ obj: NSString) -> Int {
  return CFStringGetLength(obj as CFString)
}

@_effects(readonly)
private func _copyString(_ obj: NSString) -> NSString {
  return CFStringCreateCopy(kCFAllocatorSystemDefault, obj as CFString)
}

@_effects(releasenone)
private func _getBytes(_ theString: NSString,
                       _ range: CFRange,
                       _ encoding: CFStringEncoding,
                       _ buffer: UnsafeMutablePointer<UInt8>!,
                       _ maxBufLen: CFIndex,
                       _ usedBufLen: UnsafeMutablePointer<CFIndex>!
) -> CFIndex {
  return CFStringGetBytes(
    theString as CFString,
    range,
    CFStringBuiltInEncodings.UTF8.rawValue,
    0, //lossByte,
    false, //isExternalRepresentation
    buffer,
    maxBufLen,
    usedBufLen
  )
}

private let (nscfClass, nscfConstantClass): (AnyClass, AnyClass) =
  (objc_lookUpClass("__NSCFString")!,
   objc_lookUpClass("__NSCFConstantString")!)

// This works around an ARC optimizer issue that inserts a retain-release pair
// if you do the obvious thing and just use a static let constant
@_transparent
private func _estimatedSmallStringCutoff() -> Int {
  #if arch(i386) || arch(arm)
  return 10
  #else
  return 15
  #endif
}


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
  @inline(__always)
  private static func _bridgeToSmall(
    _ source: NSString,
    _ len: Int
  ) -> String? {
    assert(len != 0)
    let result = String(uninitializedCapacity:
      _estimatedSmallStringCutoff()
    ) { ptr in
      var outCount: Int = 0
      let converted = _getBytes(
        source,
        CFRange(location: 0, length: len),
        CFStringBuiltInEncodings.UTF8.rawValue,
        UnsafeMutableRawPointer(ptr.baseAddress!).assumingMemoryBound(to:
          UInt8.self),
        _estimatedSmallStringCutoff(), //maxBufLen
        &outCount
      )
      if _slowPath(converted != len) {
        return 0 //truncated, so produce an empty String instead
      }
      return outCount
    }
    if _slowPath(result.isEmpty) {
      return nil
    }
    return result
  }
  
  @_effects(readonly)
  private static func _unconditionallyBridgeFromObjectiveC_nonTagged(
    _ source: NSString
  ) -> String {
    let len = _length(source)
    if len == 0 {
      return String()
    }
    let sourceClass:AnyClass = _getClass(source)
    
    // Only try regular CF-based NSStrings for now due to things like
    // NSLocalizedString assoc objects and NSAttributedString content proxies
    // Also only try Strings we believe will fit into a SmallString for now
    // In the future we should do mutable __NSCFStrings of any length here.
    if sourceClass == nscfClass,
       len <= _estimatedSmallStringCutoff(),
       let eager = _bridgeToSmall(source, len) {
      return eager
    } 
    
    let immutableCopy = (sourceClass == nscfConstantClass) ?
      source :
      _copyString(source)
    
    // mutable->immutable might make it start being a tagged pointer
    if _isObjCTaggedPointer(immutableCopy) {
      return _bridgeToSmall(immutableCopy, len)!
    }
    
    return _bridgeCocoaStringLazily(immutableCopy, sourceClass, len)
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> String {
    guard let source = source else {
      // `nil` has historically been used as a stand-in for an empty
      // string; map it to an empty string.
      return String()
    }
    
    if _fastPath(_isObjCTaggedPointer(source)) {
      return _bridgeToSmall(source, _length(source))!
    }
    
    return _unconditionallyBridgeFromObjectiveC_nonTagged(source)
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

  @_effects(releasenone)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> Substring {
    let str = String._unconditionallyBridgeFromObjectiveC(source)
    return str[...]
  }
}

extension String: CVarArg {}

