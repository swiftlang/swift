//===--- NSValue.swift - Bridging things in NSValue -------------*-swift-*-===//
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

extension NSRange : _ObjectiveCBridgeable {
  public static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  public static func _getObjectiveCType() -> Any.Type {
    return NSValue.self
  }

  public func _bridgeToObjectiveC() -> NSValue {
    return NSValue(range: self)
  }

  public static func _forceBridgeFromObjectiveC(
    x: NSValue,
    inout result: NSRange?
  ) {
    result = x.rangeValue
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    x: NSValue,
    inout result: NSRange?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }
}
