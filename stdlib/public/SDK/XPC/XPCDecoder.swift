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

extension xpc_object_t {
    func decodeNil(at codingPath: [CodingKey]) -> Bool {
        let nullSingleton = xpc_null_create()

        return self === nullSingleton
    }

    func decodeBool(at codingPath: [CodingKey]) throws -> Bool {
        guard xpc_get_type(self) == XPC_TYPE_BOOL else {
            throw DecodingError.typeMismatch(Bool.self,
                                             DecodingError.Context(codingPath: codingPath,
                                                                   debugDescription: "Type mismatch.",
                                                                   underlyingError: nil))
        }

        return self === XPC_BOOL_TRUE
    }

    func decodeSignedInteger<I: SignedInteger>(_ to: I.Type, at codingPath: [CodingKey]) throws -> I {
        guard xpc_get_type(self) == XPC_TYPE_INT64 else {
            throw DecodingError.typeMismatch(to.self,
                                             DecodingError.Context(codingPath: codingPath,
                                                                   debugDescription: "Type mismatch.",
                                                                   underlyingError: nil))
        }

        return to.init(xpc_int64_get_value(self))
    }

    func decodeUnsignedInteger<U: UnsignedInteger>(_ to: U.Type, at codingPath: [CodingKey]) throws -> U {
        guard xpc_get_type(self) == XPC_TYPE_UINT64 else {
            throw DecodingError.typeMismatch(to.self,
                                             DecodingError.Context(codingPath: codingPath,
                                                                   debugDescription: "Type mismatch.",
                                                                   underlyingError: nil))
        }

        return to.init(xpc_uint64_get_value(self))
    }

    func decodeFloatingPointNumber<F>(_ to: F.Type, at codingPath: [CodingKey]) throws -> F where F: BinaryFloatingPoint{
        guard xpc_get_type(self) == XPC_TYPE_DOUBLE else {
            throw DecodingError.typeMismatch(to.self,
                                             DecodingError.Context(codingPath: codingPath,
                                                                   debugDescription: "Type mismatch.",
                                                                   underlyingError: nil))
        }

        return to.init(xpc_double_get_value(self))
    }

    func decodeString(at codingPath: [CodingKey]) throws -> String {
        guard xpc_get_type(self) == XPC_TYPE_STRING else {
            throw DecodingError.typeMismatch(String.self,
                                             DecodingError.Context(codingPath: codingPath,
                                                                   debugDescription: "Type mismatch.",
                                                                   underlyingError: nil))
        }

        return String(cString: xpc_string_get_string_ptr(self)!)
    }
}
