// stdlib/public/SDK/XPC/XPCSingleValueDecodingContainer.swift -
// SingleValueDecodingContainer for XPC
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
/// This file contains a SingleValueDecodingContainer implementation for
/// xpc_object_t.
///
// -----------------------------------------------------------------------------//

public struct XPCSingleValueDecodingContainer: SingleValueDecodingContainer {
    // MARK: - Properties
    public var codingPath: [CodingKey] {
        get {
            return self.decoder.codingPath
        }
    }

    private let decoder: XPCDecoder
    private let underlyingMessage: xpc_object_t

    // MARK: - Initialization
    init(referencing decoder: XPCDecoder, wrapping xpcObject: xpc_object_t) {
        self.decoder = decoder
        self.underlyingMessage = xpcObject
    }

    public func decodeNil() -> Bool {
        return XPCDecodingHelpers.decodeNil(from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Bool.Type) throws -> Bool {
        return try XPCDecodingHelpers.decodeBool(from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: String.Type) throws -> String {
        return try XPCDecodingHelpers.decodeString(from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Double.Type) throws -> Double {
        return try XPCDecodingHelpers.decodeFloatingPointNumber(Double.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Float.Type) throws -> Float {
        return try XPCDecodingHelpers.decodeFloatingPointNumber(Float.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Int.Type) throws -> Int {
        return try XPCDecodingHelpers.decodeSignedInteger(Int.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Int8.Type) throws -> Int8 {
        return try XPCDecodingHelpers.decodeSignedInteger(Int8.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Int16.Type) throws -> Int16 {
        return try XPCDecodingHelpers.decodeSignedInteger(Int16.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Int32.Type) throws -> Int32 {
        return try XPCDecodingHelpers.decodeSignedInteger(Int32.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: Int64.Type) throws -> Int64 {
        return try XPCDecodingHelpers.decodeSignedInteger(Int64.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: UInt.Type) throws -> UInt {
        return try XPCDecodingHelpers.decodeUnsignedInteger(UInt.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: UInt8.Type) throws -> UInt8 {
        return try XPCDecodingHelpers.decodeUnsignedInteger(UInt8.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: UInt16.Type) throws -> UInt16 {
        return try XPCDecodingHelpers.decodeUnsignedInteger(UInt16.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: UInt32.Type) throws -> UInt32 {
        return try XPCDecodingHelpers.decodeUnsignedInteger(UInt32.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode(_ type: UInt64.Type) throws -> UInt64 {
        return try XPCDecodingHelpers.decodeUnsignedInteger(UInt64.self, from: self.underlyingMessage, at: self.codingPath)
    }

    public func decode<T>(_ type: T.Type) throws -> T where T : Decodable {
        return try T(from: XPCDecoder(withUnderlyingMessage: self.underlyingMessage, at: self.decoder.codingPath))
    }
}
