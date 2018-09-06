// stdlib/public/SDK/XPC/XPCDecoder.swift - Decoder impelementation for XPC
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
/// This file contains an implementation of a Decoder for xpc_object_t types, as
/// well as generic utility functions for decoding primitives that are reused by
/// all the decoding containers.
///
// -----------------------------------------------------------------------------//

public enum XPCParsingError: Error {
    case invalidXPCObjectType
    case keyDoesNotExist
    case typeMismatch
    case reachedArrayEnd
    case notYetImplemented
}

open class XPCDecoder: Decoder {
    private let underlyingMessage: xpc_object_t

    public var codingPath: [CodingKey]

    public var userInfo: [CodingUserInfoKey : Any] = [:]

    public init(withUnderlyingMessage message: xpc_object_t, at codingPath: [CodingKey] = []) {
        self.underlyingMessage = message
        self.codingPath = codingPath
    }

    public func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> where Key : CodingKey {
        let container = try XPCKeyedDecodingContainer<Key>(referencing: self, wrapping: self.underlyingMessage)
        return KeyedDecodingContainer(container)
    }

    public func unkeyedContainer() throws -> UnkeyedDecodingContainer {
        return try XPCUnkeyedDecodingContainer(referencing: self, wrapping: self.underlyingMessage)
    }

    public func singleValueContainer() throws -> SingleValueDecodingContainer {
        return XPCSingleValueDecodingContainer(referencing: self, wrapping: self.underlyingMessage)
    }

    public static func decode<T: Decodable>(_ type: T.Type, message xpcObject: xpc_object_t) throws -> T {
        return try T(from: XPCDecoder(withUnderlyingMessage: xpcObject))
    }
}

struct XPCDecodingHelpers {
    static func decodeNil(from xpcObject: xpc_object_t) -> Bool {
        let nullSingleton = xpc_null_create()

        return xpcObject === nullSingleton
    }

    static func decodeBool(from xpcObject: xpc_object_t) throws -> Bool {
        guard xpc_get_type(xpcObject) == XPC_TYPE_BOOL else {
            throw XPCParsingError.typeMismatch
        }

        return xpcObject === XPC_BOOL_TRUE
    }

    static func decodeSignedInteger<I: SignedInteger>(_ to: I.Type, from xpcObject: xpc_object_t) throws -> I {
        guard xpc_get_type(xpcObject) == XPC_TYPE_INT64 else {
            throw XPCParsingError.typeMismatch
        }

        return to.init(exactly: xpc_int64_get_value(xpcObject))!
    }

    static func decodeUnsignedInteger<I: UnsignedInteger>(_ to: I.Type, from xpcObject: xpc_object_t) throws -> I {
        guard xpc_get_type(xpcObject) == XPC_TYPE_UINT64 else {
            throw XPCParsingError.typeMismatch
        }

        return to.init(exactly: xpc_uint64_get_value(xpcObject))!
    }

    static func decodeFloatingPointNumber<F>(_ to: F.Type, from xpcObject: xpc_object_t) throws -> F where F: BinaryFloatingPoint{
        guard xpc_get_type(xpcObject) == XPC_TYPE_DOUBLE else {
            throw XPCParsingError.typeMismatch
        }

        return to.init(xpc_double_get_value(xpcObject))
    }

    static func decodeString(from xpcObject: xpc_object_t) throws -> String {
        guard xpc_get_type(xpcObject) == XPC_TYPE_STRING else {
            throw XPCParsingError.typeMismatch
        }

        return String(cString: xpc_string_get_string_ptr(xpcObject)!)
    }
}
