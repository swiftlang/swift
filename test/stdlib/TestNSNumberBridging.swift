//===--- NSNumberBridging.swift - Test bridging through NSNumber ----------===//
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
//
// RUN: %empty-directory(%t)
//
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestNSNumberBridging
// 
// RUN: %target-run %t/TestNSNumberBridging
// REQUIRES: executable_test
// REQUIRES: objc_interop

// FIXME: rdar://35814988
// UNSUPPORTED: CPU=armv7
// UNSUPPORTED: CPU=armv7s
// UNSUPPORTED: CPU=armv7k
// UNSUPPORTED: CPU=arm64
// UNSUPPORTED: CPU=arm64e

import StdlibUnittest
import Foundation
import CoreGraphics
import FoundationBridgeObjC

var nsNumberBridging = TestSuite("NSNumberBridging")

func testFloat(_ lhs: Float?, _ rhs: Float?, file: String = #file, line: UInt = #line) {
    let message = "\(String(describing: lhs)) != \(String(describing: rhs)) Float"
    if let lhsValue = lhs {
        if let rhsValue = rhs {
            if lhsValue.isNaN != rhsValue.isNaN {
                expectUnreachable(message, file: file, line: line)
            } else if lhsValue != rhsValue && !lhsValue.isNaN {
                expectUnreachable(message, file: file, line: line)
            }
        } else {
            expectUnreachable(message, file: file, line: line)
        }
    } else {
        if rhs != nil {
            expectUnreachable(message, file: file, line: line)
        }
    }
}

func testDouble(_ lhs: Double?, _ rhs: Double?, file: String = #file, line: UInt = #line) {
    let message = "\(String(describing: lhs)) != \(String(describing: rhs)) Double"
    if let lhsValue = lhs {
        if let rhsValue = rhs {
            if lhsValue.isNaN != rhsValue.isNaN {
                expectUnreachable(message, file: file, line: line)
            } else if lhsValue != rhsValue && !lhsValue.isNaN {
                expectUnreachable(message, file: file, line: line)
            }
        } else {
            expectUnreachable(message, file: file, line: line)
        }
    } else {
        if rhs != nil {
            expectUnreachable(message, file: file, line: line)
        }
    }
}


extension Int8 {
    static var _interestingValues: [Int8] {
        return [
            Int8.min,
            Int8.min + 1,
            Int8.max,
            Int8.max - 1,
            0,
            -1, 1,
            -42, 42,
        ]
    }
}

extension UInt8 {
    static var _interestingValues: [UInt8] {
        return [
            UInt8.min,
            UInt8.min + 1,
            UInt8.max,
            UInt8.max - 1,
            42,
        ]
    }
}

extension Int16 {
    static var _interestingValues: [Int16] {
        return [
            Int16.min,
            Int16.min + 1,
            Int16.max,
            Int16.max - 1,
            0,
            -1, 1,
            -42, 42,
        ]
    }
}

extension UInt16 {
    static var _interestingValues: [UInt16] {
        return [
            UInt16.min,
            UInt16.min + 1,
            UInt16.max,
            UInt16.max - 1,
            42,
        ]
    }
}

extension Int32 {
    static var _interestingValues: [Int32] {
        return [
            Int32.min,
            Int32.min + 1,
            Int32.max,
            Int32.max - 1,
            0,
            -1, 1,
            -42, 42,
        ]
    }
}

extension UInt32 {
    static var _interestingValues: [UInt32] {
        return [
            UInt32.min,
            UInt32.min + 1,
            UInt32.max,
            UInt32.max - 1,
            42,
        ]
    }
}

extension Int64 {
    static var _interestingValues: [Int64] {
        return [
            Int64.min,
            Int64.min + 1,
            Int64.max,
            Int64.max - 1,
            0,
            -1, 1,
            -42, 42,
        ]
    }
}

extension UInt64 {
    static var _interestingValues: [UInt64] {
        return [
            UInt64.min,
            UInt64.min + 1,
            UInt64.max,
            UInt64.max - 1,
            42,
        ]
    }
}

extension Int {
    static var _interestingValues: [Int] {
        return [
            Int.min,
            Int.min + 1,
            Int.max,
            Int.max - 1,
            0,
            -1, 1,
            -42, 42,
        ]
    }
}

extension UInt {
    static var _interestingValues: [UInt] {
        return [
            UInt.min,
            UInt.min + 1,
            UInt.max,
            UInt.max - 1,
            42,
        ]
    }
}

extension Float {
    static var _interestingValues: [Float] {
        return [
            -Float.infinity,
            -Float.greatestFiniteMagnitude,
            -1.0,
            -Float.ulpOfOne,
            -Float.leastNormalMagnitude,
            -0.0,
            0.0,
            Float.leastNormalMagnitude,
            Float.ulpOfOne,
            1.0,
            Float.greatestFiniteMagnitude,
            Float.infinity,
            Float.nan,
        ]
    }
}

extension Double {
    static var _interestingValues: [Double] {
        return [
            -Double.infinity,
            -Double.greatestFiniteMagnitude,
            -1.0,
            -Double.ulpOfOne,
            -Double.leastNormalMagnitude,
            -0.0,
            0.0,
            Double.leastNormalMagnitude,
            Double.ulpOfOne,
            1.0,
            Double.greatestFiniteMagnitude,
            Double.infinity,
            Double.nan,
        ]
    }
}

extension CGFloat {
    static var _interestingValues: [CGFloat] {
        return [
            -CGFloat.infinity,
            -CGFloat.greatestFiniteMagnitude,
            -1.0,
            -CGFloat.ulpOfOne,
            -CGFloat.leastNormalMagnitude,
            -0.0,
            0.0,
            CGFloat.leastNormalMagnitude,
            CGFloat.ulpOfOne,
            1.0,
            CGFloat.greatestFiniteMagnitude,
            CGFloat.infinity,
            CGFloat.nan,
        ]
    }
}

extension Bool {
    static var _interestingValues: [Bool] {
        return [false, true]
    }
}

func testNSNumberBridgeFromInt8() {
    for interestingValue in Int8._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            let float = (number!) as? Float
            expectEqual(Float(interestingValue), float)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromUInt8() {
    for interestingValue in UInt8._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            let float = (number!) as? Float
            expectEqual(Float(interestingValue), float)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromInt16() {
    for interestingValue in Int16._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            let float = (number!) as? Float
            expectEqual(Float(interestingValue), float)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromUInt16() {
    for interestingValue in UInt8._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            let float = (number!) as? Float
            expectEqual(Float(interestingValue), float)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromInt32() {
    for interestingValue in Int32._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            
            let float = (number!) as? Float
            let expectedFloat = Float(exactly: int32!)
            testFloat(expectedFloat, float)
            
            let double = (number!) as? Double
            let expectedDouble = Double(int32!)
            testDouble(expectedDouble, double)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromUInt32() {
    for interestingValue in UInt32._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
            
            let float = (number!) as? Float
            let expectedFloat = Float(exactly: uint32!)
            testFloat(expectedFloat, float)
          
            let double = (number!) as? Double
            let expectedDouble = Double(uint32!)
            testDouble(expectedDouble, double)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromInt64() {
    for interestingValue in Int64._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromUInt64() {
    for interestingValue in UInt64._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromInt() {
    for interestingValue in Int._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromUInt() {
    for interestingValue in UInt._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)

            let float = (number!) as? Float
            let expectedFloat = Float(exactly: uint!)
            if UInt.bitWidth == 32 && uint! >= UInt.max - 1 {
              expectNil(expectedFloat)
            } else {
              testFloat(expectedFloat, float)
            }
          
            let double = (number!) as? Double
            let expectedDouble = Double(exactly: uint!)
            testDouble(expectedDouble, double)
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromFloat() {
    for interestingValue in Float._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)

            let float = (number!) as? Float
            let expectedFloat = Float(exactly: interestingValue)
            if interestingValue.isNaN {
                expectTrue(float?.isNaN == true)
                expectNil(expectedFloat)
            } else {
                testFloat(expectedFloat, float)
            }
            
            let double = (number!) as? Double
            let expectedDouble = Double(exactly: interestingValue)
            if interestingValue.isNaN {
                expectTrue(double?.isNaN == true)
                expectNil(expectedDouble)
            } else {
                testDouble(expectedDouble, double)
            }
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromDouble() {
    for interestingValue in Double._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue), uint)

            let float = (number!) as? Float
            let expectedFloat = Float(exactly: interestingValue)
            if interestingValue.isNaN {
                expectTrue(float?.isNaN == true)
                expectNil(expectedFloat)
            } else {
                testFloat(expectedFloat, float)
            }
            
            let double = (number!) as? Double
            let expectedDouble = Double(exactly: interestingValue)
            if interestingValue.isNaN {
                expectTrue(double?.isNaN == true)
                expectNil(expectedDouble)
            } else {
                testDouble(expectedDouble, double)
            }
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue)
        testNumber(created)
    }
}

func testNSNumberBridgeFromCGFloat() {
    for interestingValue in CGFloat._interestingValues {
        func testNumber(_ number: NSNumber?) {
            expectNotNil(number)
            let int8 = (number!) as? Int8
            expectEqual(Int8(exactly: interestingValue.native), int8)
            let uint8 = (number!) as? UInt8
            expectEqual(UInt8(exactly: interestingValue.native), uint8)
            let int16 = (number!) as? Int16
            expectEqual(Int16(exactly: interestingValue.native), int16)
            let uint16 = (number!) as? UInt16
            expectEqual(UInt16(exactly: interestingValue.native), uint16)
            let int32 = (number!) as? Int32
            expectEqual(Int32(exactly: interestingValue.native), int32)
            let uint32 = (number!) as? UInt32
            expectEqual(UInt32(exactly: interestingValue.native), uint32)
            let int64 = (number!) as? Int64
            expectEqual(Int64(exactly: interestingValue.native), int64)
            let uint64 = (number!) as? UInt64
            expectEqual(UInt64(exactly: interestingValue.native), uint64)
            let int = (number!) as? Int
            expectEqual(Int(exactly: interestingValue.native), int)
            let uint = (number!) as? UInt
            expectEqual(UInt(exactly: interestingValue.native), uint)
            
            let float = (number!) as? Float
            let expectedFloat = Float(exactly: interestingValue.native)
            if interestingValue.isNaN {
                expectTrue(float?.isNaN == true)
                expectNil(expectedFloat)
            } else {
                testFloat(expectedFloat, float)
            }
            
            let double = (number!) as? Double
            let expectedDouble = Double(exactly: interestingValue.native)
            if interestingValue.isNaN {
                expectTrue(double?.isNaN == true)
                expectNil(expectedDouble)
            } else {
                testDouble(expectedDouble, double)
            }
        }
        let bridged = interestingValue as NSNumber
        testNumber(bridged)
        let created = NSNumber(value: interestingValue.native)
        testNumber(created)
    }
}

func test_numericBitPatterns_to_floatingPointTypes() {
    let signed_numbers: [NSNumber] = [
        NSNumber(value: Int64(6)),
        NSNumber(value: Int64(bitPattern: 1 << 56)),
        NSNumber(value: Int64(bitPattern: 1 << 53)),
        NSNumber(value: Int64(bitPattern: 1 << 52)),
        NSNumber(value: Int64(bitPattern: 1 << 25)),
        NSNumber(value: Int64(bitPattern: 1 << 24)),
        NSNumber(value: Int64(bitPattern: 1 << 23)),
        NSNumber(value: -Int64(bitPattern: 1 << 53)),
        NSNumber(value: -Int64(bitPattern: 1 << 52)),
        NSNumber(value: -Int64(6)),
        NSNumber(value: -Int64(bitPattern: 1 << 56)),
        NSNumber(value: -Int64(bitPattern: 1 << 25)),
        NSNumber(value: -Int64(bitPattern: 1 << 24)),
        NSNumber(value: -Int64(bitPattern: 1 << 23)),
    ]

    let signed_values: [Int64] = [
        Int64(6),
        Int64(bitPattern: 1 << 56),
        Int64(bitPattern: 1 << 53),
        Int64(bitPattern: 1 << 52),
        Int64(bitPattern: 1 << 25),
        Int64(bitPattern: 1 << 24),
        Int64(bitPattern: 1 << 23),
        -Int64(bitPattern: 1 << 53),
        -Int64(bitPattern: 1 << 52),
        -Int64(6),
        -Int64(bitPattern: 1 << 56),
        -Int64(bitPattern: 1 << 25),
        -Int64(bitPattern: 1 << 24),
        -Int64(bitPattern: 1 << 23),
    ]

    let unsigned_numbers: [NSNumber] = [
        NSNumber(value: UInt64(bitPattern: 6)),
        NSNumber(value: UInt64(bitPattern: 1 << 56)),
        NSNumber(value: UInt64(bitPattern: 1 << 63)),
        NSNumber(value: UInt64(bitPattern: 1 << 53)),
        NSNumber(value: UInt64(bitPattern: 1 << 52)),
        NSNumber(value: UInt64(bitPattern: 1 << 25)),
        NSNumber(value: UInt64(bitPattern: 1 << 24)),
        NSNumber(value: UInt64(bitPattern: 1 << 23)),
    ]

    let unsigned_values: [UInt64] = [
        UInt64(bitPattern: 6),
        UInt64(bitPattern: 1 << 56),
        UInt64(bitPattern: 1 << 63),
        UInt64(bitPattern: 1 << 53),
        UInt64(bitPattern: 1 << 52),
        UInt64(bitPattern: 1 << 25),
        UInt64(bitPattern: 1 << 24),
        UInt64(bitPattern: 1 << 23)
    ]

    for (number, value) in zip(signed_numbers, signed_values) {
        let numberCast = Double(exactly: number)
        let valueCast = Double(exactly: value)
        expectEqual(numberCast, valueCast)
    }

    for (number, value) in zip(unsigned_numbers, unsigned_values) {
        let numberCast = Double(exactly: number)
        let valueCast = Double(exactly: value)
        expectEqual(numberCast, valueCast)
    }

    for (number, value) in zip(signed_numbers, signed_values) {
        let numberCast = Float(exactly: number)
        let valueCast = Float(exactly: value)
        expectEqual(numberCast, valueCast)
    }

    for (number, value) in zip(unsigned_numbers, unsigned_values) {
        let numberCast = Float(exactly: number)
        let valueCast = Float(exactly: value)
        expectEqual(numberCast, valueCast)
    }
}

func testNSNumberBridgeAnyHashable() {
    var dict = [AnyHashable : Any]()
    for i in -Int(UInt8.min) ... Int(UInt8.max) {
        dict[i] = "\(i)"
    }

    // When bridging a dictionary to NSDictionary, we should be able to access
    // the keys through either an Int (the original type boxed in AnyHashable)
    // or NSNumber (the type Int bridged to).
    let ns_dict = dict as NSDictionary
    for i in -Int(UInt8.min) ... Int(UInt8.max) {
        guard let value = ns_dict[i] as? String else {
            expectUnreachable("Unable to look up value by Int key.")
            continue
        }

        guard let ns_value = ns_dict[NSNumber(value: i)] as? String else {
            expectUnreachable("Unable to look up value by NSNumber key.")
            continue
        }
        
        expectEqual(value, ns_value)
    }
}

func testNSNumberBridgeAnyHashableObjc() {
    let range = -Int(UInt8.min) ... Int(UInt8.max)
    var dict = [AnyHashable : Any]()
    for i in range {
        dict[i] = "\(i)"
    }

    let verifier = NumberBridgingTester()
    expectTrue(verifier.verifyKeys(in: NSRange(range), existIn: dict))
}

nsNumberBridging.test("Bridge Int8") { testNSNumberBridgeFromInt8() }
nsNumberBridging.test("Bridge UInt8") { testNSNumberBridgeFromUInt8() }
nsNumberBridging.test("Bridge Int16") { testNSNumberBridgeFromInt16() }
nsNumberBridging.test("Bridge UInt16") { testNSNumberBridgeFromUInt16() }
nsNumberBridging.test("Bridge Int32") { testNSNumberBridgeFromInt32() }
nsNumberBridging.test("Bridge UInt32") { testNSNumberBridgeFromUInt32() }
nsNumberBridging.test("Bridge Int64") { testNSNumberBridgeFromInt64() }
nsNumberBridging.test("Bridge UInt64") { testNSNumberBridgeFromUInt64() }
nsNumberBridging.test("Bridge Int") { testNSNumberBridgeFromInt() }
nsNumberBridging.test("Bridge UInt") { testNSNumberBridgeFromUInt() }
nsNumberBridging.test("Bridge Float") { testNSNumberBridgeFromFloat() }
nsNumberBridging.test("Bridge Double") { testNSNumberBridgeFromDouble() }
nsNumberBridging.test("Bridge CGFloat") { testNSNumberBridgeFromCGFloat() }
nsNumberBridging.test("bitPattern to exactly") { test_numericBitPatterns_to_floatingPointTypes() }
nsNumberBridging.test("Bridge AnyHashable") { testNSNumberBridgeAnyHashable() }
nsNumberBridging.test("Bridge AnyHashable (ObjC)") { testNSNumberBridgeAnyHashableObjc() }
runAllTests()
