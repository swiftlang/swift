//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import _SwiftCoreFoundationOverlayShims

extension Decimal {
    public typealias RoundingMode = NSDecimalNumber.RoundingMode
    public typealias CalculationError = NSDecimalNumber.CalculationError
}

public func pow(_ x: Decimal, _ y: Int) -> Decimal {
    var x = x
    var result = Decimal()
    NSDecimalPower(&result, &x, y, .plain)
    return result
}

extension Decimal : Hashable, Comparable {
    private subscript(index: UInt32) -> UInt16 {
        get {
            switch index {
            case 0: return _mantissa.0
            case 1: return _mantissa.1
            case 2: return _mantissa.2
            case 3: return _mantissa.3
            case 4: return _mantissa.4
            case 5: return _mantissa.5
            case 6: return _mantissa.6
            case 7: return _mantissa.7
            default: fatalError("Invalid index \(index) for _mantissa")
            }
        }
    }
    
    internal var doubleValue: Double {
        if _length == 0 {
            return _isNegative == 1 ? Double.nan : 0
        }

        var d = 0.0
        for idx in (0..<min(_length, 8)).reversed() {
            d = d * 65536 + Double(self[idx])
        }
        
        if _exponent < 0 {
            for _ in _exponent..<0 {
                d /= 10.0
            }
        } else {
            for _ in 0..<_exponent {
                d *= 10.0
            }
        }
        return _isNegative != 0 ? -d : d
    }

    public func hash(into hasher: inout Hasher) {
        // FIXME: This is a weak hash.  We should rather normalize self to a
        // canonical member of the exact same equivalence relation that
        // NSDecimalCompare implements, then simply feed all components to the
        // hasher.
        hasher.combine(doubleValue)
    }

    public static func ==(lhs: Decimal, rhs: Decimal) -> Bool {
        var lhsVal = lhs
        var rhsVal = rhs
        // Note: In swift-corelibs-foundation, a bitwise comparison is first
        // performed using fileprivate members not accessible here.
        return NSDecimalCompare(&lhsVal, &rhsVal) == .orderedSame
    }

    public static func <(lhs: Decimal, rhs: Decimal) -> Bool {
        var lhsVal = lhs
        var rhsVal = rhs
        return NSDecimalCompare(&lhsVal, &rhsVal) == .orderedAscending
    }
}

extension Decimal : CustomStringConvertible {
    public init?(string: __shared String, locale: __shared Locale? = nil) {
        let scan = Scanner(string: string)
        var theDecimal = Decimal()
        scan.locale = locale
        if !scan.scanDecimal(&theDecimal) {
            return nil
        }
        self = theDecimal
    }

    // Note: In swift-corelibs-foundation, `NSDecimalString(_:_:)` is
    // implemented in terms of `description`; here, it's the other way around.
    public var description: String {
        var value = self
        return NSDecimalString(&value, nil)
    }
}

extension Decimal : Codable {
    private enum CodingKeys : Int, CodingKey {
        case exponent
        case length
        case isNegative
        case isCompact
        case mantissa
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let exponent = try container.decode(CInt.self, forKey: .exponent)
        let length = try container.decode(CUnsignedInt.self, forKey: .length)
        let isNegative = try container.decode(Bool.self, forKey: .isNegative)
        let isCompact = try container.decode(Bool.self, forKey: .isCompact)

        var mantissaContainer = try container.nestedUnkeyedContainer(forKey: .mantissa)
        var mantissa: (CUnsignedShort, CUnsignedShort, CUnsignedShort, CUnsignedShort,
                       CUnsignedShort, CUnsignedShort, CUnsignedShort, CUnsignedShort) = (0,0,0,0,0,0,0,0)
        mantissa.0 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.1 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.2 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.3 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.4 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.5 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.6 = try mantissaContainer.decode(CUnsignedShort.self)
        mantissa.7 = try mantissaContainer.decode(CUnsignedShort.self)

        self = Decimal(_exponent: exponent,
                       _length: length,
                       _isNegative: CUnsignedInt(isNegative ? 1 : 0),
                       _isCompact: CUnsignedInt(isCompact ? 1 : 0),
                       _reserved: 0,
                       _mantissa: mantissa)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(_exponent, forKey: .exponent)
        try container.encode(_length, forKey: .length)
        try container.encode(_isNegative == 0 ? false : true, forKey: .isNegative)
        try container.encode(_isCompact == 0 ? false : true, forKey: .isCompact)

        var mantissaContainer = container.nestedUnkeyedContainer(forKey: .mantissa)
        try mantissaContainer.encode(_mantissa.0)
        try mantissaContainer.encode(_mantissa.1)
        try mantissaContainer.encode(_mantissa.2)
        try mantissaContainer.encode(_mantissa.3)
        try mantissaContainer.encode(_mantissa.4)
        try mantissaContainer.encode(_mantissa.5)
        try mantissaContainer.encode(_mantissa.6)
        try mantissaContainer.encode(_mantissa.7)
    }
}

extension Decimal : ExpressibleByFloatLiteral {
    public init(floatLiteral value: Double) {
        self.init(value)
    }
}

extension Decimal : ExpressibleByIntegerLiteral {
    public init(integerLiteral value: Int) {
        self.init(value)
    }
}

extension Decimal : SignedNumeric {
    public var magnitude: Decimal {
        guard _length != 0 else { return self }
        return Decimal(
            _exponent: self._exponent, _length: self._length,
            _isNegative: 0, _isCompact: self._isCompact,
            _reserved: 0, _mantissa: self._mantissa)
    }

    // FIXME(integers): implement properly
    public init?<T : BinaryInteger>(exactly source: T) {
        fatalError()
    }

    public static func +=(lhs: inout Decimal, rhs: Decimal) {
        var rhs = rhs
        _ = withUnsafeMutablePointer(to: &lhs) {
            NSDecimalAdd($0, $0, &rhs, .plain)
        }
    }

    public static func -=(lhs: inout Decimal, rhs: Decimal) {
        var rhs = rhs
        _ = withUnsafeMutablePointer(to: &lhs) {
            NSDecimalSubtract($0, $0, &rhs, .plain)
        }
    }

    public static func *=(lhs: inout Decimal, rhs: Decimal) {
        var rhs = rhs
        _ = withUnsafeMutablePointer(to: &lhs) {
            NSDecimalMultiply($0, $0, &rhs, .plain)
        }
    }

    public static func /=(lhs: inout Decimal, rhs: Decimal) {
        var rhs = rhs
        _ = withUnsafeMutablePointer(to: &lhs) {
            NSDecimalDivide($0, $0, &rhs, .plain)
        }
    }

    public static func +(lhs: Decimal, rhs: Decimal) -> Decimal {
        var answer = lhs
        answer += rhs
        return answer
    }

    public static func -(lhs: Decimal, rhs: Decimal) -> Decimal {
        var answer = lhs
        answer -= rhs
        return answer
    }

    public static func *(lhs: Decimal, rhs: Decimal) -> Decimal {
        var answer = lhs
        answer *= rhs
        return answer
    }

    public static func /(lhs: Decimal, rhs: Decimal) -> Decimal {
        var answer = lhs
        answer /= rhs
        return answer
    }

    public mutating func negate() {
        guard _length != 0 else { return }
        _isNegative = _isNegative == 0 ? 1 : 0
    }
}

extension Decimal {
    @available(swift, obsoleted: 4, message: "Please use arithmetic operators instead")
    @_transparent
    public mutating func add(_ other: Decimal) {
        self += other
    }

    @available(swift, obsoleted: 4, message: "Please use arithmetic operators instead")
    @_transparent
    public mutating func subtract(_ other: Decimal) {
        self -= other
    }

    @available(swift, obsoleted: 4, message: "Please use arithmetic operators instead")
    @_transparent
    public mutating func multiply(by other: Decimal) {
        self *= other
    }

    @available(swift, obsoleted: 4, message: "Please use arithmetic operators instead")
    @_transparent
    public mutating func divide(by other: Decimal) {
        self /= other
    }
}

extension Decimal : Strideable {
    public func distance(to other: Decimal) -> Decimal {
        return self - other
    }

    public func advanced(by n: Decimal) -> Decimal {
        return self + n
    }
}

// The methods in this extension exist to match the protocol requirements of
// FloatingPoint, even if we can't conform directly.
//
// If it becomes clear that conformance is truly impossible, we can deprecate
// some of the methods (e.g. `isEqual(to:)` in favor of operators).
extension Decimal {
    public static let leastFiniteMagnitude = Decimal(
        _exponent: 127,
        _length: 8,
        _isNegative: 1,
        _isCompact: 1,
        _reserved: 0,
        _mantissa: (0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff)
    )

    public static let greatestFiniteMagnitude = Decimal(
        _exponent: 127,
        _length: 8,
        _isNegative: 0,
        _isCompact: 1,
        _reserved: 0,
        _mantissa: (0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff)
    )

    public static let leastNormalMagnitude = Decimal(
        _exponent: -127,
        _length: 1,
        _isNegative: 0,
        _isCompact: 1,
        _reserved: 0,
        _mantissa: (0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000)
    )

    public static let leastNonzeroMagnitude = Decimal(
        _exponent: -127,
        _length: 1,
        _isNegative: 0,
        _isCompact: 1,
        _reserved: 0,
        _mantissa: (0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000)
    )

    public static let pi = Decimal(
        _exponent: -38,
        _length: 8,
        _isNegative: 0,
        _isCompact: 1,
        _reserved: 0,
        _mantissa: (0x6623, 0x7d57, 0x16e7, 0xad0d, 0xaf52, 0x4641, 0xdfa7, 0xec58)
    )

    @available(*, unavailable, message: "Decimal does not yet fully adopt FloatingPoint.")
    public static var infinity: Decimal { fatalError("Decimal does not yet fully adopt FloatingPoint") }

    @available(*, unavailable, message: "Decimal does not yet fully adopt FloatingPoint.")
    public static var signalingNaN: Decimal { fatalError("Decimal does not yet fully adopt FloatingPoint") }

    public static var quietNaN: Decimal {
        return Decimal(
            _exponent: 0, _length: 0, _isNegative: 1, _isCompact: 0,
            _reserved: 0, _mantissa: (0, 0, 0, 0, 0, 0, 0, 0))
    }

    public static var nan: Decimal { quietNaN }

    public static var radix: Int { 10 }

    public init(_ value: UInt8) {
        self.init(UInt64(value))
    }
    
    public init(_ value: Int8) {
        self.init(Int64(value))
    }
    
    public init(_ value: UInt16) {
        self.init(UInt64(value))
    }
    
    public init(_ value: Int16) {
        self.init(Int64(value))
    }
    
    public init(_ value: UInt32) {
        self.init(UInt64(value))
    }
    
    public init(_ value: Int32) {
        self.init(Int64(value))
    }

    public init(_ value: UInt64) {
        self = Decimal()
        if value == 0 {
            return
        }

        var compactValue = value
        var exponent: Int32 = 0
        while compactValue % 10 == 0 {
            compactValue /= 10
            exponent += 1
        }
        _isCompact = 1
        _exponent = exponent

        let wordCount = ((UInt64.bitWidth - compactValue.leadingZeroBitCount) + (UInt16.bitWidth - 1)) / UInt16.bitWidth
        _length = UInt32(wordCount)
        _mantissa.0 = UInt16(truncatingIfNeeded: compactValue >> 0)
        _mantissa.1 = UInt16(truncatingIfNeeded: compactValue >> 16)
        _mantissa.2 = UInt16(truncatingIfNeeded: compactValue >> 32)
        _mantissa.3 = UInt16(truncatingIfNeeded: compactValue >> 48)
    }
    
    public init(_ value: Int64) {
        self.init(value.magnitude)
        if value < 0 {
            _isNegative = 1
        }
    }
    
    public init(_ value: UInt) {
        self.init(UInt64(value))
    }
    
    public init(_ value: Int) {
        self.init(Int64(value))
    }

    public init(_ value: Double) {
        precondition(!value.isInfinite, "Decimal does not yet fully adopt FloatingPoint")
        if value.isNaN {
            self = Decimal.nan
        } else if value == 0.0 {
            self = Decimal()
        } else {
            self = Decimal()
            let negative = value < 0
            var val = negative ? -1 * value : value
            var exponent = 0
            while val < Double(UInt64.max - 1) {
                val *= 10.0
                exponent -= 1
            }
            while Double(UInt64.max - 1) < val {
                val /= 10.0
                exponent += 1
            }
            var mantissa = UInt64(val)

            var i: UInt32 = 0
            // This is a bit ugly but it is the closest approximation of the C
            // initializer that can be expressed here.
            while mantissa != 0 && i < 8 /* NSDecimalMaxSize */ {
                switch i {
                case 0:
                    _mantissa.0 = UInt16(truncatingIfNeeded: mantissa)
                case 1:
                    _mantissa.1 = UInt16(truncatingIfNeeded: mantissa)
                case 2:
                    _mantissa.2 = UInt16(truncatingIfNeeded: mantissa)
                case 3:
                    _mantissa.3 = UInt16(truncatingIfNeeded: mantissa)
                case 4:
                    _mantissa.4 = UInt16(truncatingIfNeeded: mantissa)
                case 5:
                    _mantissa.5 = UInt16(truncatingIfNeeded: mantissa)
                case 6:
                    _mantissa.6 = UInt16(truncatingIfNeeded: mantissa)
                case 7:
                    _mantissa.7 = UInt16(truncatingIfNeeded: mantissa)
                default:
                    fatalError("initialization overflow")
                }
                mantissa = mantissa >> 16
                i += 1
            }
            _length = i
            _isNegative = negative ? 1 : 0
            _isCompact = 0
            _exponent = Int32(exponent)
            NSDecimalCompact(&self)
        }
    }

    public init(sign: FloatingPointSign, exponent: Int, significand: Decimal) {
        self.init(
            _exponent: Int32(exponent) + significand._exponent,
            _length: significand._length,
            _isNegative: sign == .plus ? 0 : 1,
            _isCompact: significand._isCompact,
            _reserved: 0,
            _mantissa: significand._mantissa)
    }

    public init(signOf: Decimal, magnitudeOf magnitude: Decimal) {
        self.init(
            _exponent: magnitude._exponent,
            _length: magnitude._length,
            _isNegative: signOf._isNegative,
            _isCompact: magnitude._isCompact,
            _reserved: 0,
            _mantissa: magnitude._mantissa)
    }

    public var exponent: Int {
        return Int(_exponent)
    }

    public var significand: Decimal {
        return Decimal(
            _exponent: 0, _length: _length, _isNegative: _isNegative, _isCompact: _isCompact,
            _reserved: 0, _mantissa: _mantissa)
    }

    public var sign: FloatingPointSign {
        return _isNegative == 0 ? FloatingPointSign.plus : FloatingPointSign.minus
    }

    public var ulp: Decimal {
        if !self.isFinite { return Decimal.nan }
        return Decimal(
            _exponent: _exponent, _length: 8, _isNegative: 0, _isCompact: 1,
            _reserved: 0, _mantissa: (0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000))
    }

    public var nextUp: Decimal {
        return self + Decimal(
            _exponent: _exponent, _length: 1, _isNegative: 0, _isCompact: 1,
            _reserved: 0, _mantissa: (0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000))
    }

    public var nextDown: Decimal {
        return self - Decimal(
            _exponent: _exponent, _length: 1, _isNegative: 0, _isCompact: 1,
            _reserved: 0, _mantissa: (0x0001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000))
    }

    /// The IEEE 754 "class" of this type.
    public var floatingPointClass: FloatingPointClassification {
        if _length == 0 && _isNegative == 1 {
            return .quietNaN
        } else if _length == 0 {
            return .positiveZero
        }
        // NSDecimal does not really represent normal and subnormal in the same
        // manner as the IEEE standard, for now we can probably claim normal for
        // any nonzero, non-NaN values.
        if _isNegative == 1 {
            return .negativeNormal
        } else {
            return .positiveNormal
        }
    }

    public var isCanonical: Bool { true }

    /// `true` if `self` is negative, `false` otherwise.
    public var isSignMinus: Bool { _isNegative != 0 }

    /// `true` if `self` is +0.0 or -0.0, `false` otherwise.
    public var isZero: Bool { _length == 0 && _isNegative == 0 }

    /// `true` if `self` is subnormal, `false` otherwise.
    public var isSubnormal: Bool { false }

    /// `true` if `self` is normal (not zero, subnormal, infinity, or NaN),
    /// `false` otherwise.
    public var isNormal: Bool { !isZero && !isInfinite && !isNaN }

    /// `true` if `self` is zero, subnormal, or normal (not infinity or NaN),
    /// `false` otherwise.
    public var isFinite: Bool { !isNaN }

    /// `true` if `self` is infinity, `false` otherwise.
    public var isInfinite: Bool { false }

    /// `true` if `self` is NaN, `false` otherwise.
    public var isNaN: Bool { _length == 0 && _isNegative == 1 }

    /// `true` if `self` is a signaling NaN, `false` otherwise.
    public var isSignaling: Bool { false }

    /// `true` if `self` is a signaling NaN, `false` otherwise.
    public var isSignalingNaN: Bool { false }

    public func isEqual(to other: Decimal) -> Bool {
        var lhs = self
        var rhs = other
        return NSDecimalCompare(&lhs, &rhs) == .orderedSame
    }

    public func isLess(than other: Decimal) -> Bool {
        var lhs = self
        var rhs = other
        return NSDecimalCompare(&lhs, &rhs) == .orderedAscending
    }

    public func isLessThanOrEqualTo(_ other: Decimal) -> Bool {
        var lhs = self
        var rhs = other
        let order = NSDecimalCompare(&lhs, &rhs)
        return order == .orderedAscending || order == .orderedSame
    }

    public func isTotallyOrdered(belowOrEqualTo other: Decimal) -> Bool {
        // Note: Decimal does not have -0 or infinities to worry about
        if self.isNaN {
            return false
        }
        if self < other {
            return true
        }
        if other < self {
            return false
        }
        // Fall through to == behavior
        return true
    }

    @available(*, unavailable, message: "Decimal does not yet fully adopt FloatingPoint.")
    public mutating func formTruncatingRemainder(dividingBy other: Decimal) { fatalError("Decimal does not yet fully adopt FloatingPoint") }
}

extension Decimal : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSDecimalNumber {
        return NSDecimalNumber(decimal: self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSDecimalNumber, result: inout Decimal?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSDecimalNumber, result: inout Decimal?) -> Bool {
        result = input.decimalValue
        return true
    }

    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSDecimalNumber?) -> Decimal {
        guard let src = source else { return Decimal(_exponent: 0, _length: 0, _isNegative: 0, _isCompact: 0, _reserved: 0, _mantissa: (0, 0, 0, 0, 0, 0, 0, 0)) }
        return src.decimalValue
    }
}
