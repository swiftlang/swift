// stdlib/public/SDK/XPC/XPCKeyedDecodingContainer.swift -
// KeyedDecodingContainer implementation for XPC
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
//
// This file contains a KeyedDecodingContainer implementation for xpc_object_t.
//
// -----------------------------------------------------------------------------//

public struct XPCKeyedDecodingContainer<K: CodingKey>: KeyedDecodingContainerProtocol {
    public typealias Key = K

    // MARK: - Properties

    /// A reference to the decoder we're reading from.
    private let decoder: XPCDecoder

    /// The path of coding keys taken to get to this point in decoding.
    public var codingPath: [CodingKey] {
        get {
            return self.decoder.codingPath
        }
    }

    private let underlyingMessage: xpc_object_t

    // MARK: - Initialization

    /// Initializes `self` by referencing the given decoder and container.
    init(referencing decoder: XPCDecoder, wrapping underlyingMessage: xpc_object_t) throws {
        guard xpc_get_type(underlyingMessage) == XPC_TYPE_DICTIONARY else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Did not find xpc dictionary in keyed container"))
        }
        self.decoder = decoder
        self.underlyingMessage = underlyingMessage
    }

    // MARK: - Helpers
    private func decodeIntegerType<I: SignedInteger & FixedWidthInteger>(_ type: I.Type, forKey key: Key) throws -> I {
        let foundValue = try getXPCObject(for: key)

        return try foundValue.decodeSignedInteger(type.self, at: self.codingPath)
    }

    private func decodeIntegerType<I: UnsignedInteger & FixedWidthInteger>(_ type: I.Type, forKey key: Key) throws -> I {
        let foundValue = try getXPCObject(for: key)

        return try foundValue.decodeUnsignedInteger(type.self, at: self.codingPath)
    }

    private func decodeFloatingPointType<F: BinaryFloatingPoint>(_ type: F.Type, forKey key: Key) throws -> F {
        let foundValue = try getXPCObject(for: key)

        return try foundValue.decodeFloatingPointNumber(type.self, at: self.codingPath)
    }

    private func getXPCObject(for key: CodingKey) throws -> xpc_object_t {
        guard let foundValue = key.stringValue.withCString({
            return xpc_dictionary_get_value(self.underlyingMessage, $0)
        }) else {
            throw DecodingError.keyNotFound(key,
                                            DecodingError.Context(
                                              codingPath: self.codingPath,
                                              debugDescription: "Could not find key \(key.stringValue)"))
        }

        return foundValue
    }

    // MARK: - KeyedDecodingContainerProtocol Methods

    /// You shouldn't rely on this because this is slow
    public var allKeys: [Key] {
        get {
            var keys: [Key] = []
            xpc_dictionary_apply(self.underlyingMessage) { (key, _) -> Bool in
                keys.append(Key(stringValue: String(cString: key))!)
                return true
            }
            return keys
        }
    }

    public func contains(_ key: Key) -> Bool {
        do {
            let _ = try getXPCObject(for: key)
        } catch {
            return false
        }
        return true
    }

    public func decodeNil(forKey key: Key) throws -> Bool {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }
        let foundValue = try getXPCObject(for: key)

        return foundValue.decodeNil(at: self.codingPath)
    }

    public func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        return try foundValue.decodeBool(at: self.codingPath)
    }


    public func decode(_ type: Int.Type, forKey key: Key) throws -> Int {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64 {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeIntegerType(type, forKey: key)
    }

    public func decode(_ type: Float.Type, forKey key: Key) throws -> Float {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeFloatingPointType(type, forKey: key)
    }

    public func decode(_ type: Double.Type, forKey key: Key) throws -> Double {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        return try decodeFloatingPointType(type, forKey: key)
    }

    public func decode(_ type: String.Type, forKey key: Key) throws -> String {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        return try foundValue.decodeString(at: self.codingPath)
    }

    public func decode<T: Decodable>(_ type: T.Type, forKey key: Key) throws -> T {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        return try T(from: XPCDecoder(withUnderlyingMessage: foundValue, at: self.decoder.codingPath))
    }

    public func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        let container = try XPCKeyedDecodingContainer<NestedKey>(referencing: decoder, wrapping: foundValue)
        return KeyedDecodingContainer<NestedKey>(container)
    }

    public func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        return try XPCUnkeyedDecodingContainer(referencing: decoder, wrapping: foundValue)
    }

    public func superDecoder() throws -> Decoder {
        self.decoder.codingPath.append(XPCCodingKey.superKey)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: XPCCodingKey.superKey)

        return XPCDecoder(withUnderlyingMessage: foundValue, at: self.decoder.codingPath)
    }

    public func superDecoder(forKey key: Key) throws -> Decoder {
        self.decoder.codingPath.append(key)
        defer { self.decoder.codingPath.removeLast() }

        let foundValue = try getXPCObject(for: key)

        return XPCDecoder(withUnderlyingMessage: foundValue, at: self.decoder.codingPath)
    }
}

