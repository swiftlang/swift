// stdlib/public/SDK/XPC/XPCUnkeyedEncodingContainer.swift -
// UnkeyedEncodingContainer for XPC
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
/// This file contains a UnkeyedEncodingContainer implementation for
/// xpc_object_t. It also includes a specialization of XPCEncoder for handling
/// the super class case as we don't know what kind of object is going to be
/// needed and we need to maitain a reference to the underlying array.
///
// -----------------------------------------------------------------------------//

public struct XPCUnkeyedEncodingContainer: UnkeyedEncodingContainer {
    // MARK: - Properties
    public var codingPath: [CodingKey] {
        get {
            return self.encoder.codingPath
        }
    }

    public var count: Int {
        get {
            return xpc_array_get_count(underlyingMessage)
        }
    }

    private let encoder: XPCEncoder

    private let underlyingMessage: xpc_object_t

    // MARK: - Initialization
    init(referencing encoder: XPCEncoder, wrapping underlyingMessage: xpc_object_t) throws {
        self.encoder = encoder

        guard xpc_get_type(underlyingMessage) == XPC_TYPE_ARRAY else {
            throw XPCEncodingHelpers.makeEncodingError(underlyingMessage, encoder.codingPath, "Internal error")
        }

        self.underlyingMessage = underlyingMessage
    }

    // MARK: - UnkeyedEncodingContainer protocol methods
    public mutating func encode(_ value: Bool) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let boolValue = XPCEncodingHelpers.encodeBool(value)
        xpc_array_append_value(self.underlyingMessage, boolValue)
    }

    public mutating func encodeNil() throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let nilValue = XPCEncodingHelpers.encodeNil()
        xpc_array_append_value(self.underlyingMessage, nilValue)
    }

    public mutating func encode(_ value: String) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let stringValue = XPCEncodingHelpers.encodeString(value)
        xpc_array_append_value(self.underlyingMessage, stringValue)
    }

    public mutating func encode(_ value: Double) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let doubleValue = XPCEncodingHelpers.encodeDouble(value)
        xpc_array_append_value(self.underlyingMessage, doubleValue)
    }

    public mutating func encode(_ value: Float) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let floatValue = XPCEncodingHelpers.encodeFloat(value)
        xpc_array_append_value(self.underlyingMessage, floatValue)
    }

    public mutating func encode(_ value: Int) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let intValue = XPCEncodingHelpers.encodeSignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, intValue)
    }

    public mutating func encode(_ value: Int8) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let intValue = XPCEncodingHelpers.encodeSignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, intValue)
    }

    public mutating func encode(_ value: Int16) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let intValue = XPCEncodingHelpers.encodeSignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, intValue)
    }

    public mutating func encode(_ value: Int32) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let intValue = XPCEncodingHelpers.encodeSignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, intValue)
    }

    public mutating func encode(_ value: Int64) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let intValue = XPCEncodingHelpers.encodeSignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, intValue)
    }

    public mutating func encode(_ value: UInt) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let unsignedValue = XPCEncodingHelpers.encodeUnsignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, unsignedValue)
    }

    public mutating func encode(_ value: UInt8) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let unsignedValue = XPCEncodingHelpers.encodeUnsignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, unsignedValue)
    }

    public mutating func encode(_ value: UInt16) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let unsignedValue = XPCEncodingHelpers.encodeUnsignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, unsignedValue)
    }

    public mutating func encode(_ value: UInt32) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let unsignedValue = XPCEncodingHelpers.encodeUnsignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, unsignedValue)
    }

    public mutating func encode(_ value: UInt64) throws {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let unsignedValue = XPCEncodingHelpers.encodeUnsignedInteger(value)
        xpc_array_append_value(self.underlyingMessage, unsignedValue)
    }

    public mutating func encode<T>(_ value: T) throws where T : Encodable {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        do {
            let xpcObject = try XPCEncoder.encode(value, at: self.encoder.codingPath)
            xpc_array_append_value(self.underlyingMessage, xpcObject)
        } catch let error as EncodingError {
            throw error
        } catch {
            throw XPCEncodingHelpers.makeEncodingError(value, self.codingPath, String(describing: error))
        }
    }

    public mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey> where NestedKey : CodingKey {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let xpcDictionary = xpc_dictionary_create(nil, nil, 0)
        xpc_array_append_value(self.underlyingMessage, xpcDictionary)

        //It is OK to force this because we are explicitly passing a dictionary
        let container = try! XPCKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: xpcDictionary)
        return KeyedEncodingContainer(container)
    }

    public mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        let xpcArray = xpc_array_create(nil, 0)
        xpc_array_append_value(self.underlyingMessage, xpcArray)

        return try! XPCUnkeyedEncodingContainer(referencing: self.encoder, wrapping: xpcArray)
    }

    public mutating func superEncoder() -> Encoder {
        self.encoder.codingPath.append(XPCCodingKey(intValue: self.count - 1)!)
        defer { self.encoder.codingPath.removeLast() }

        // Insert dummy value in array so we don't get bit latter
        xpc_array_append_value(self.underlyingMessage, XPCEncodingHelpers.encodeNil())
        return XPCArrayReferencingEncoder(at: self.codingPath, wrapping: self.underlyingMessage, forIndex: self.count - 1)
    }
}

// This is used for encoding super classes, we don't know yet what kind of
// container the caller will request so we can not prepoluate in superEncoder().
// To overcome this we alias the encoder, the underlying array, this way we can
// insert the key-value pair upon request and use the encoder to maintain the
// encoding state
private class XPCArrayReferencingEncoder: XPCEncoder {
    let xpcArray: xpc_object_t
    let index: Int

    init(at codingPath: [CodingKey], wrapping array: xpc_object_t, forIndex index: Int) {
        self.xpcArray = array
        self.index = index
        super.init(at: codingPath)
    }

    override func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> where Key : CodingKey {
        let newDictionary = xpc_dictionary_create(nil, nil, 0)
        xpc_array_set_value(self.xpcArray, self.index, newDictionary)

        // It is OK to force this through because we are explicitly passing a dictionary
        let container = try! XPCKeyedEncodingContainer<Key>(referencing: self, wrapping: newDictionary)
        return KeyedEncodingContainer(container)
    }

    override func unkeyedContainer() -> UnkeyedEncodingContainer {
        let newArray = xpc_array_create(nil, 0)
        xpc_array_set_value(self.xpcArray, self.index, newArray)

        // It is OK to force this through because we are explicitly passing an array
        return try! XPCUnkeyedEncodingContainer(referencing: self, wrapping: newArray)
    }

    override func singleValueContainer() -> SingleValueEncodingContainer {
        // It is OK to force this through because we are explictly passing an array
        return XPCSingleValueEncodingContainer(referencing: self, insertionClosure: {
            value in
                guard self.index < xpc_array_get_count(self.xpcArray) else {
                    throw XPCEncodingHelpers.makeEncodingError(value, [], "Internal Error")
                }
                xpc_array_set_value(self.xpcArray, self.index, value)
        })
    }
}
