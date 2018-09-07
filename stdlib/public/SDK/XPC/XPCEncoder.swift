// stdlib/public/SDK/XPC/XPCEncoder.swift - Encoder implementation for XPC
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
/// This file contains stuff an Encoder implementation for xpc_object_t as well
/// as generic utility functions for encoding primiives that are reused by the
/// encoding containers.
///
// -----------------------------------------------------------------------------//

public enum XPCSerializationError: Error {
    case invalidXPCObjectType
    case insertionPastEndOfArray
    case notYetImplemented
}

public class XPCEncoder: Encoder {
    private enum ContainerKind: String {
        case Keyed
        case Unkeyed
        case SingleValue
        case None
    }

    public var codingPath: [CodingKey]

    public var userInfo: [CodingUserInfoKey : Any] = [:]

    var topLevelContainer: xpc_object_t?

    private var containerKind: ContainerKind = .None

    public init(at codingPath: [CodingKey] = []) {
        self.codingPath = codingPath
    }

    public func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> where Key : CodingKey {
        switch self.containerKind {
        case .None:
            self.topLevelContainer = xpc_dictionary_create(nil, nil, 0)
            self.containerKind = .Keyed
        case .Keyed:
            break
        default:
            preconditionFailure("This encoder already has a container of kind \(self.containerKind)")
        }

        // It is OK to force this because we are explicitly passing a dictionary
        let container = try! XPCKeyedEncodingContainer<Key>(referencing: self, wrapping: self.topLevelContainer!)
        return KeyedEncodingContainer(container)
    }

    public func unkeyedContainer() -> UnkeyedEncodingContainer {
        switch self.containerKind {
        case .None:
            self.topLevelContainer = xpc_array_create(nil, 0)
            self.containerKind = .Unkeyed
        case .Unkeyed:
            break
        default:
            preconditionFailure("This encoder already has a container of kind \(self.containerKind)")
        }

        //It is OK to force this through becasue we are explicitly passing an array
        return try! XPCUnkeyedEncodingContainer(referencing: self, wrapping: self.topLevelContainer!)
    }

    public func singleValueContainer() -> SingleValueEncodingContainer {
        switch self.containerKind {
        case .None:
            self.containerKind = .SingleValue
        default:
            preconditionFailure("This encoder already has a container of kind \(self.containerKind)")
        }

        let inserter = XPCSingleValueEncoderInserter(into: self)

        return XPCSingleValueEncodingContainer(referencing: self, inserter: inserter)
    }

    public static func encode<T: Encodable>(_ value: T, at codingPath: [CodingKey] = []) throws -> xpc_object_t {
        let encoder = XPCEncoder(at: codingPath)
        try value.encode(to: encoder)
        return encoder.topLevelContainer!
    }
}

struct XPCEncodingHelpers {
    static func encodeNil() -> xpc_object_t {
        return xpc_null_create()
    }

    static func encodeBool(_ value: Bool) -> xpc_object_t {
        return xpc_bool_create(value)
    }

    static func encodeSignedInteger<I: SignedInteger>(_ value: I) -> xpc_object_t {
        return xpc_int64_create(Int64(exactly: value)!)
    }

    static func encodeUnsignedInteger<U: UnsignedInteger>(_ value: U) -> xpc_object_t {
        return xpc_uint64_create(UInt64(exactly: value)!)
    }

    static func encodeDouble(_ value: Double) -> xpc_object_t {
        return xpc_double_create(Double(value))
    }

    static func encodeFloat(_ value: Float) -> xpc_object_t {
        return xpc_double_create(Double(value))
    }

    static func encodeString(_ value: String) -> xpc_object_t {
        return value.withCString({ return xpc_string_create($0) })
    }
}
