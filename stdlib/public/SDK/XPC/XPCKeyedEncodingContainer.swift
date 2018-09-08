// stdlib/public/SDK/XPC/XPCKeyedEncodingContainer.swift -
// KeyedEncodingContainer implementation for XPC
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
/// This file contains a KeyedEncodingContainer implementation for xpc_object_t.
/// This includes a specialization of the XPCEncoder for handling the super
/// class case, as we don't know what container type is going to be needed to
/// handle it.
///
// -----------------------------------------------------------------------------//

public struct XPCKeyedEncodingContainer<K: CodingKey>: KeyedEncodingContainerProtocol {
    public typealias Key = K

    // MARK: - Properties

    /// A reference to the encoder we're writing to.
    private let encoder: XPCEncoder

    private let underlyingMesage: xpc_object_t

    /// The path of coding keys taken to get to this point in encoding.
    public var codingPath: [CodingKey] {
        get {
            return self.encoder.codingPath
        }
    }

    // MARK: - Initialization

    /// Initializes `self` with the given references.
    init(referencing encoder: XPCEncoder, wrapping dictionary: xpc_object_t) throws {
        self.encoder = encoder
        guard xpc_get_type(dictionary) == XPC_TYPE_DICTIONARY else {
            throw XPCEncodingHelpers.makeEncodingError(dictionary, encoder.codingPath, "Internal error")
        }
        self.underlyingMesage = dictionary
    }

    // MARK: - KeyedEncodingContainerProtocol Methods

    public mutating func encodeNil(forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeNil())})
    }

    public mutating func encode(_ value: Bool, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_bool(self.underlyingMesage, $0, value) })
    }

    public mutating func encode(_ value: Int, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeSignedInteger(value)) })
    }

    public mutating func encode(_ value: Int8, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeSignedInteger(value)) })
    }

    public mutating func encode(_ value: Int16, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeSignedInteger(value)) })
    }

    public mutating func encode(_ value: Int32, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeSignedInteger(value)) })
    }

    public mutating func encode(_ value: Int64, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeSignedInteger(value)) })
    }

    public mutating func encode(_ value: UInt, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeUnsignedInteger(value)) })
    }

    public mutating func encode(_ value: UInt8, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeUnsignedInteger(value)) })
    }

    public mutating func encode(_ value: UInt16, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeUnsignedInteger(value)) })
    }

    public mutating func encode(_ value: UInt32, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeUnsignedInteger(value)) })
    }

    public mutating func encode(_ value: UInt64, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeUnsignedInteger(value)) })
    }

    public mutating func encode(_ value: String, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeString(value)) })
    }

    public mutating func encode(_ value: Float, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeFloat(value)) })
    }

    public mutating func encode(_ value: Double, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, XPCEncodingHelpers.encodeDouble(value)) })
    }

    public mutating func encode<T : Encodable>(_ value: T, forKey key: Key) throws {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        do {
            let xpcObject = try XPCEncoder.encode(value, at: self.encoder.codingPath)
            key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, xpcObject) })
        } catch let error as EncodingError {
            throw error
        } catch {
            throw XPCEncodingHelpers.makeEncodingError(value, self.codingPath, String(describing: error))
        }
    }

    public mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        let xpcDictionary = xpc_dictionary_create(nil, nil, 0)
        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, xpcDictionary) })
        // It is OK to force this through because we know we are providing a dictionary
        let container = try! XPCKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: xpcDictionary)
        return KeyedEncodingContainer(container)
    }

    public mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        let xpcArray = xpc_array_create(nil, 0)
        key.stringValue.withCString({ xpc_dictionary_set_value(self.underlyingMesage, $0, xpcArray) })
        let container = try! XPCUnkeyedEncodingContainer(referencing: self.encoder, wrapping: xpcArray)
        return container
    }

    public mutating func superEncoder() -> Encoder {
        self.encoder.codingPath.append(XPCCodingKey.superKey)
        defer { self.encoder.codingPath.removeLast() }

        return XPCDictionaryReferencingEncoder(at: self.encoder.codingPath, wrapping: self.underlyingMesage, forKey: XPCCodingKey.superKey)
    }

    public mutating func superEncoder(forKey key: Key) -> Encoder {
        self.encoder.codingPath.append(key)
        defer { self.encoder.codingPath.removeLast() }

        return XPCDictionaryReferencingEncoder(at: self.encoder.codingPath, wrapping: self.underlyingMesage, forKey: key)
    }
}

// This is used for encoding super classes, we don't know yet what kind of
// container the caller will request so we can not prepoluate in superEncoder().
// To overcome this we alias the encoder, the underlying dictionary, and the key
// to use, this way we can insert the key-value pair upon request and use the
// encoder to maintain the encoding state
fileprivate class XPCDictionaryReferencingEncoder: XPCEncoder {
    let xpcDictionary: xpc_object_t
    let key: CodingKey

    init(at codingPath: [CodingKey], wrapping dictionary: xpc_object_t, forKey key: CodingKey) {
        self.xpcDictionary = dictionary
        self.key = key
        super.init(at: codingPath)
    }

    override func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> where Key : CodingKey {
        let newDictionary = xpc_dictionary_create(nil, nil, 0)
        self.key.stringValue.withCString({ xpc_dictionary_set_value(self.xpcDictionary, $0, newDictionary) })

        // It is OK to force this through because we are explicitly passing a dictionary
        let container = try! XPCKeyedEncodingContainer<Key>(referencing: self, wrapping: newDictionary)
        return KeyedEncodingContainer(container)
    }

    override func unkeyedContainer() -> UnkeyedEncodingContainer {
        let newArray = xpc_array_create(nil, 0)
        self.key.stringValue.withCString({ xpc_dictionary_set_value(self.xpcDictionary, $0, newArray) })

        // It is OK to force this through because we are explicitly passing an array
        return try! XPCUnkeyedEncodingContainer(referencing: self, wrapping: newArray)
    }

    override func singleValueContainer() -> SingleValueEncodingContainer {
        // It is OK to force this through because we are explicitly passing a dictionary
        let inserter = try! XPCSingleValueDictionaryInserter(into: self.xpcDictionary, at: self.key)
        return XPCSingleValueEncodingContainer(referencing: self, inserter: inserter)
    }
}
