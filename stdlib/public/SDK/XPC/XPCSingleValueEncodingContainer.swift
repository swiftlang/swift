// stdlib/public/SDK/XPC/XPCSingleValueEncodingContainer.swift -
// SingleValueEncodingContainer for XPC
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
/// This file contains a SincludeValueEncodingContainer implementation for
/// xpc_object_t.
///
// -----------------------------------------------------------------------------//

protocol SingleValueEncodingContainerInsertion {
    mutating func insert(_ value: xpc_object_t) throws
}

struct XPCSingleValueDictionaryInserter: SingleValueEncodingContainerInsertion {
    private let dictionary: xpc_object_t
    private let key: CodingKey

    init(into dictionary: xpc_object_t, at key: CodingKey) throws {
        guard xpc_get_type(dictionary) == XPC_TYPE_DICTIONARY else {
            throw XPCSerializationError.invalidXPCObjectType
        }
        self.dictionary = dictionary
        self.key = key
    }

    mutating func insert(_ value: xpc_object_t) {
        self.key.stringValue.withCString({ xpc_dictionary_set_value(self.dictionary, $0, value)})
    }
}

struct XPCSingleValueArrayInserter: SingleValueEncodingContainerInsertion {
    private let array: xpc_object_t
    private let index: Int

    init(into array: xpc_object_t, at index: Int) throws {
        guard xpc_get_type(array) == XPC_TYPE_ARRAY else {
            throw XPCSerializationError.invalidXPCObjectType
        }
        self.array = array
        self.index = index
    }

    mutating func insert(_ value: xpc_object_t) throws {
        guard self.index < xpc_array_get_count(self.array) else {
            throw XPCSerializationError.insertionPastEndOfArray
        }
        xpc_array_set_value(self.array, self.index, value)
    }
}

struct XPCSingleValueEncoderInserter: SingleValueEncodingContainerInsertion {
    private let encoder: XPCEncoder

    init(into encoder: XPCEncoder) {
        self.encoder = encoder
    }

    mutating func insert(_ value: xpc_object_t) throws {
        self.encoder.topLevelContainer = value
    }
}

public struct XPCSingleValueEncodingContainer: SingleValueEncodingContainer {
    // MARK: - Properties
    public var codingPath: [CodingKey] {
        get {
            return self.encoder.codingPath
        }
    }

    private let encoder: XPCEncoder
    private var inserter: SingleValueEncodingContainerInsertion

    // MARK: - Initialization
    init(referencing encoder: XPCEncoder, inserter: SingleValueEncodingContainerInsertion) {
        self.encoder = encoder
        self.inserter = inserter
    }

    // MARK: - SingleValueEncodingContainer protocol methods
    public mutating func encodeNil() throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeNil())
    }

    public mutating func encode(_ value: Bool) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeBool(value))
    }

    public mutating func encode(_ value: String) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeString(value))
    }

    public mutating func encode(_ value: Double) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeDouble(value))
    }

    public mutating func encode(_ value: Float) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeFloat(value))
    }

    public mutating func encode(_ value: Int) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int8) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int16) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int32) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int64) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: UInt) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt8) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt16) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt32) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt64) throws {
        try self.inserter.insert(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode<T>(_ value: T) throws where T : Encodable {
        let xpcObject = try XPCEncoder.encode(value, at: self.encoder.codingPath)
        try self.inserter.insert(xpcObject)
    }
}
