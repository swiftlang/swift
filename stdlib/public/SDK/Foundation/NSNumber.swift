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
import CoreGraphics

extension Int8 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.int8Value
    }

    public init(truncating number: NSNumber) {
        self = number.int8Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.int8Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Int8?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Int8?) -> Bool {
        guard let value = Int8(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Int8 {
        var result: Int8?
        guard let src = source else { return Int8(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Int8(0) }
        return result!
    }
}

extension UInt8 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.uint8Value
    }

    public init(truncating number: NSNumber) {
        self = number.uint8Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.uint8Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt8?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt8?) -> Bool {
        guard let value = UInt8(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt8 {
        var result: UInt8?
        guard let src = source else { return UInt8(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return UInt8(0) }
        return result!
    }
}

extension Int16 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.int16Value
    }

    public init(truncating number: NSNumber) {
        self = number.int16Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.int16Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Int16?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Int16?) -> Bool {
        guard let value = Int16(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Int16 {
        var result: Int16?
        guard let src = source else { return Int16(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Int16(0) }
        return result!
    }
}

extension UInt16 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.uint16Value
    }

    public init(truncating number: NSNumber) {
        self = number.uint16Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.uint16Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt16?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt16?) -> Bool {
        guard let value = UInt16(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt16 {
        var result: UInt16?
        guard let src = source else { return UInt16(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return UInt16(0) }
        return result!
    }
}

extension Int32 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.int32Value
    }

    public init(truncating number: NSNumber) {
        self = number.int32Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.int32Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Int32?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Int32?) -> Bool {
        guard let value = Int32(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Int32 {
        var result: Int32?
        guard let src = source else { return Int32(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Int32(0) }
        return result!
    }
}

extension UInt32 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.uint32Value
    }

    public init(truncating number: NSNumber) {
        self = number.uint32Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.uint32Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt32?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt32?) -> Bool {
        guard let value = UInt32(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt32 {
        var result: UInt32?
        guard let src = source else { return UInt32(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return UInt32(0) }
        return result!
    }
}

extension Int64 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.int64Value
    }

    public init(truncating number: NSNumber) {
        self = number.int64Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.int64Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Int64?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Int64?) -> Bool {
        guard let value = Int64(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Int64 {
        var result: Int64?
        guard let src = source else { return Int64(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Int64(0) }
        return result!
    }
}

extension UInt64 : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.uint64Value
    }

    public init(truncating number: NSNumber) {
        self = number.uint64Value
    }

    public init?(exactly number: NSNumber) {
        let value = number.uint64Value
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt64?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt64?) -> Bool {
        guard let value = UInt64(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt64 {
        var result: UInt64?
        guard let src = source else { return UInt64(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return UInt64(0) }
        return result!
    }
}

extension Int : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.intValue
    }

    public init(truncating number: NSNumber) {
        self = number.intValue
    }

    public init?(exactly number: NSNumber) {
        let value = number.intValue
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Int?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Int?) -> Bool {
        guard let value = Int(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Int {
        var result: Int?
        guard let src = source else { return Int(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Int(0) }
        return result!
    }
}

extension UInt : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.uintValue
    }

    public init(truncating number: NSNumber) {
        self = number.uintValue
    }

    public init?(exactly number: NSNumber) {
        let value = number.uintValue
        guard NSNumber(value: value) == number else { return nil }
        self = value
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt?) -> Bool {
        guard let value = UInt(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt {
        var result: UInt?
        guard let src = source else { return UInt(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return UInt(0) }
        return result!
    }
}

extension Float : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.floatValue
    }

    public init(truncating number: NSNumber) {
        self = number.floatValue
    }

    public init?(exactly number: NSNumber) {
        guard let value = Double(exactly: number) else { return nil }
        guard let result = Float(exactly: value) else { return nil }
        self = result
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Float?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Float?) -> Bool {
        guard let value = Double(exactly: x) else { return false }
        guard !value.isNaN else {
            result = Float.nan
            return true
        }
        guard !value.isInfinite else {
            if value.sign == .minus {
                result = -Float.infinity
            } else {
                result = Float.infinity
            }
            return true
        }
        guard Swift.abs(value) <= Double(Float.greatestFiniteMagnitude) else {
            return false
        }
        
        result = Float(value)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Float {
        var result: Float?
        guard let src = source else { return Float(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Float(0) }
        return result!
    }
}

extension Double : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.doubleValue
    }

    public init(truncating number: NSNumber) {
        self = number.doubleValue
    }

    public init?(exactly number: NSNumber) {
        let type = number.objCType.pointee
        if type == 0x51 {
            guard let result = Double(exactly: number.uint64Value) else { return nil }
            self = result
        } else if type == 0x71 {
            guard let result = Double(exactly: number.int64Value) else  { return nil }
            self = result
        } else {
            self = number.doubleValue
        }
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Double?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Double?) -> Bool {
        guard let value = Double(exactly: x) else { return false }
        result = value
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Double {
        var result: Double?
        guard let src = source else { return Double(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return Double(0) }
        return result!
    }
}

extension Bool : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        self = number.boolValue
    }

    public init(truncating number: NSNumber) {
        self = number.boolValue
    }

    public init?(exactly number: NSNumber) {
        if number === kCFBooleanTrue as NSNumber || NSNumber(value: 1) == number {
            self = true
        } else if number === kCFBooleanFalse as NSNumber || NSNumber(value: 0) == number {
            self = false
        } else {
            return nil
        }
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout Bool?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout Bool?) -> Bool {
        if x === kCFBooleanTrue as NSNumber || NSNumber(value: 1) == x {
            result = true
            return true
        } else if x === kCFBooleanFalse as NSNumber || NSNumber(value: 0) == x {
            result = false
            return true
        }
        
        return false
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> Bool {
        var result: Bool?
        guard let src = source else { return false }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return false }
        return result!
    }
}

extension CGFloat : _ObjectiveCBridgeable {
    @available(swift, deprecated: 4, renamed: "init(truncating:)")
    public init(_ number: NSNumber) {
        native = CGFloat.NativeType(truncating: number)
    }

    public init(truncating number: NSNumber) {
        native = CGFloat.NativeType(truncating: number)
    }

    public init?(exactly number: NSNumber) {
        var nativeValue: CGFloat.NativeType? = 0
        guard CGFloat.NativeType._conditionallyBridgeFromObjectiveC(number, result: &nativeValue) else { return nil }
        self.native = nativeValue!
    }

    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNumber {
        return NSNumber(value: self.native)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout CGFloat?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout CGFloat?) -> Bool {
        var nativeValue: CGFloat.NativeType? = 0
        guard CGFloat.NativeType._conditionallyBridgeFromObjectiveC(x, result: &nativeValue) else { return false }
        result = CGFloat(nativeValue!)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> CGFloat {
        var result: CGFloat?
        guard let src = source else { return CGFloat(0) }
        guard _conditionallyBridgeFromObjectiveC(src, result: &result) else { return CGFloat(0) }
        return result!
    }
}

// Literal support for NSNumber
extension NSNumber : ExpressibleByFloatLiteral, ExpressibleByIntegerLiteral, ExpressibleByBooleanLiteral {
    /// Create an instance initialized to `value`.
    @nonobjc
    public required convenience init(integerLiteral value: Int) {
        self.init(value: value)
    }

    /// Create an instance initialized to `value`.
    @nonobjc
    public required convenience init(floatLiteral value: Double) {
        self.init(value: value)
    }

    /// Create an instance initialized to `value`.
    @nonobjc
    public required convenience init(booleanLiteral value: Bool) {
        self.init(value: value)
    }
}

extension NSNumber : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to prevent infinite recursion trying to bridge
    // AnyHashable to NSObject.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        if let nsDecimalNumber: NSDecimalNumber = self as? NSDecimalNumber {
            return AnyHashable(nsDecimalNumber.decimalValue)
        } else if self === kCFBooleanTrue as NSNumber {
            return AnyHashable(true)
        } else if self === kCFBooleanFalse as NSNumber {
            return AnyHashable(false)
        } else if NSNumber(value: int8Value) == self {
            return AnyHashable(int8Value)
        } else  if NSNumber(value: uint8Value) == self {
            return AnyHashable(uint8Value)
        } else if NSNumber(value: int16Value) == self {
            return AnyHashable(int16Value)
        } else if NSNumber(value: uint16Value) == self {
            return AnyHashable(uint16Value)
        } else if NSNumber(value: int32Value) == self {
            return AnyHashable(int32Value)
        } else if NSNumber(value: uint32Value) == self {
            return AnyHashable(uint32Value)
        } else if NSNumber(value: int64Value) == self {
            return AnyHashable(int64Value)
        } else if NSNumber(value: floatValue) == self {
            return AnyHashable(floatValue)
        } else if NSNumber(value: doubleValue) == self {
            return AnyHashable(doubleValue)
        } else {
            return nil
        }
    }
}

