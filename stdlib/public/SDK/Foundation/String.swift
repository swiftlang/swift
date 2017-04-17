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
    result = _bridgeFromObjectiveCImpl(x)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSString,
    result: inout String?
  ) -> Bool {
    result = self._bridgeFromObjectiveCImpl(x)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSString?
  ) -> String {
    return _bridgeFromObjectiveCImpl(source)
  }
}

extension String: CVarArg {}
