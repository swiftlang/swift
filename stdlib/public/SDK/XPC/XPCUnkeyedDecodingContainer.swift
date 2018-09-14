// stdlib/public/SDK/XPC/XPCUnkeyedDecodingContainer.swift -
// UnkeyedDecodingContainer for XPC
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// This file contains a UnkeyedDecodingContainer implementation for
/// xpc_object_t.
///
// -----------------------------------------------------------------------------//

public struct XPCUnkeyedDecodingContainer: UnkeyedDecodingContainer {

    // MARK: - Properties
    public var codingPath: [CodingKey] {
        get {
            return self.decoder.codingPath
        }
    }

    public var count: Int?

    public var isAtEnd: Bool {
        get {
            return self.currentIndex >= self.count!
        }
    }

    public private(set) var currentIndex: Int

    private let underlyingMessage: xpc_object_t

    private let decoder: XPCDecoder

    // MARK: - Initilization
    init(referencing decoder: XPCDecoder, wrapping: xpc_object_t) throws {
        guard xpc_get_type(wrapping) == XPC_TYPE_ARRAY else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Did not find xpc array in unkeyed container."))
        }

        self.underlyingMessage = wrapping
        self.decoder = decoder
        self.count = xpc_array_get_count(self.underlyingMessage)
        self.currentIndex = 0
    }

    // MARK: - Helpers
    private mutating func decodeIntegerType<I: SignedInteger>(_ type: I.Type) throws -> I {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let integer: I = try foundValue.decodeSignedInteger(type.self, at: self.codingPath)
        self.currentIndex += 1
        return integer
    }

    private mutating func decodeIntegerType<I: UnsignedInteger>(_ type: I.Type) throws -> I {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let integer: I = try foundValue.decodeUnsignedInteger(type.self, at: self.codingPath)
        self.currentIndex += 1
        return integer
    }

    private mutating func decodeFloatingPointType<F: BinaryFloatingPoint>(_ type: F.Type) throws -> F {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let float: F = try foundValue.decodeFloatingPointNumber(type.self, at: self.codingPath)
        self.currentIndex += 1
        return float
    }

    // MARK: - UnkeyedDecodingContainer protocol methods
    public mutating func decodeNil() throws -> Bool {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        if foundValue.decodeNil(at: self.codingPath) {
            self.currentIndex += 1
            return true
        }
        return false
    }

    public mutating func decode(_ type: Bool.Type) throws -> Bool {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let boolean = try foundValue.decodeBool(at: self.codingPath)
        self.currentIndex += 1
        return boolean
    }

    public mutating func decode(_ type: String.Type) throws -> String {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let string = try foundValue.decodeString(at: self.codingPath)
        self.currentIndex += 1
        return string
    }

    public mutating func decode(_ type: Double.Type) throws -> Double {
        return try decodeFloatingPointType(type)
    }

    public mutating func decode(_ type: Float.Type) throws -> Float {
        return try decodeFloatingPointType(type)
    }

    public mutating func decode(_ type: Int.Type) throws -> Int {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: Int8.Type) throws -> Int8 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: Int16.Type) throws -> Int16 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: Int32.Type) throws -> Int32 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: Int64.Type) throws -> Int64 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: UInt.Type) throws -> UInt {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: UInt8.Type) throws -> UInt8 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: UInt16.Type) throws -> UInt16 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: UInt32.Type) throws -> UInt32 {
        return try decodeIntegerType(type)
    }

    public mutating func decode(_ type: UInt64.Type) throws -> UInt64 {
        return try decodeIntegerType(type)
    }

    public mutating func decode<T>(_ type: T.Type) throws -> T where T : Decodable {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let constructedValue = try T(from: XPCDecoder(withUnderlyingMessage: foundValue, at: self.decoder.codingPath))
        self.currentIndex += 1
        return constructedValue
    }

    public mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey> where NestedKey : CodingKey {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let container = try XPCKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: foundValue)
        self.currentIndex += 1
        return KeyedDecodingContainer(container)
    }

    public mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)

        let container = try XPCUnkeyedDecodingContainer(referencing: self.decoder, wrapping: foundValue)
        self.currentIndex += 1
        return container
    }

    public mutating func superDecoder() throws -> Decoder {
        guard !self.isAtEnd else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
                                                                    debugDescription: "Reached end of unkeyed container."))
        }
        self.decoder.codingPath.append(XPCCodingKey(intValue: self.currentIndex)!)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = xpc_array_get_value(self.underlyingMessage, self.currentIndex)
        self.currentIndex += 1
        return XPCDecoder(withUnderlyingMessage: foundValue, at: self.decoder.codingPath)
    }
}
