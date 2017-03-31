//===----------------------------------------------------------------------===//
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

//===----------------------------------------------------------------------===//
// Codable
//===----------------------------------------------------------------------===//

/// Conformance to `Encodable` indicates that a type can encode itself to an external representation.
public protocol Encodable {
    /// Encodes `self` into the given encoder.
    ///
    /// If `self` fails to encode anything, `encoder` will encode an empty keyed container in its place.
    ///
    /// - parameter encoder: The encoder to write data to.
    /// - throws: An error if any values are invalid for `encoder`'s format.
    func encode(to encoder: Encoder) throws
}

/// Conformance to `Decodable` indicates that a type can decode itself from an external representation.
public protocol Decodable {
    /// Initializes `self` by decoding from `decoder`.
    ///
    /// - parameter decoder: The decoder to read data from.
    /// - throws: An error if reading from the decoder fails, or if read data is corrupted or otherwise invalid.
    init(from decoder: Decoder) throws
}

/// Conformance to `Codable` indicates that a type can convert itself into and out of an external representation.
public typealias Codable = Encodable & Decodable

//===----------------------------------------------------------------------===//
// CodingKey
//===----------------------------------------------------------------------===//

/// Conformance to `CodingKey` indicates that a type can be used as a key for encoding and decoding.
public protocol CodingKey {
    /// The string to use in a named collection (e.g. a string-keyed dictionary).
    var stringValue: String { get }

    /// Initializes `self` from a string.
    ///
    /// - parameter stringValue: The string value of the desired key.
    /// - returns: An instance of `Self` from the given string, or `nil` if the given string does not correspond to any instance of `Self`.
    init?(stringValue: String)

    /// The int to use in an indexed collection (e.g. an int-keyed dictionary).
    var intValue: Int? { get }

    /// Initializes `self` from an integer.
    ///
    /// - parameter intValue: The integer value of the desired key.
    /// - returns: An instance of `Self` from the given integer, or `nil` if the given integer does not correspond to any instance of `Self`.
    init?(intValue: Int)
}

//===----------------------------------------------------------------------===//
// Encoder & Decoder
//===----------------------------------------------------------------------===//

/// An `Encoder` is a type which can encode values into a native format for external representation.
public protocol Encoder {
    /// The path of coding keys taken to get to this point in encoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// Any contextual information set by the user for encoding.
    var userInfo: [CodingUserInfoKey : Any] { get }

    /// Returns an encoding container appropriate for holding multiple values keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - returns: A new keyed encoding container.
    /// - precondition: May not be called after a prior `self.unkeyedContainer()` call.
    /// - precondition: May not be called after a value has been encoded through a previous `self.singleValueContainer()` call.
    func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key>

    /// Returns an encoding container appropriate for holding multiple unkeyed values.
    ///
    /// - returns: A new empty unkeyed container.
    /// - precondition: May not be called after a prior `self.container(keyedBy:)` call.
    /// - precondition: May not be called after a value has been encoded through a previous `self.singleValueContainer()` call.
    func unkeyedContainer() -> UnkeyedEncodingContainer

    /// Returns an encoding container appropriate for holding a single primitive value.
    ///
    /// - returns: A new empty single value container.
    /// - precondition: May not be called after a prior `self.container(keyedBy:)` call.
    /// - precondition: May not be called after a prior `self.unkeyedContainer()` call.
    /// - precondition: May not be called after a value has been encoded through a previous `self.singleValueContainer()` call.
    func singleValueContainer() -> SingleValueEncodingContainer
}

/// A `Decoder` is a type which can decode values from a native format into in-memory representations.
public protocol Decoder {
    /// The path of coding keys taken to get to this point in decoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// Any contextual information set by the user for decoding.
    var userInfo: [CodingUserInfoKey : Any] { get }

    /// Returns the data stored in `self` as represented in a container keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a keyed container.
    func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key>

    /// Returns the data stored in `self` as represented in a container appropriate for holding values with no keys.
    ///
    /// - returns: An unkeyed container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not an unkeyed container.
    func unkeyedContainer() throws -> UnkeyedDecodingContainer

    /// Returns the data stored in `self` as represented in a container appropriate for holding a single primitive value.
    ///
    /// - returns: A single value container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a single value container.
    func singleValueContainer() throws -> SingleValueDecodingContainer
}

//===----------------------------------------------------------------------===//
// Keyed Encoding Containers
//===----------------------------------------------------------------------===//

/// Conformance to `KeyedEncodingContainerProtocol` indicates that a type provides a view into an `Encoder`'s storage and is used to hold the encoded properties of an `Encodable` type in a keyed manner.
///
/// Encoders should provide types conforming to `KeyedEncodingContainerProtocol` for their format.
public protocol KeyedEncodingContainerProtocol {
    associatedtype Key : CodingKey

    /// The path of coding keys taken to get to this point in encoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode<T : Encodable>(_ value: T?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Bool?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int8?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int16?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int32?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int64?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt8?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt16?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt32?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt64?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Float?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Double?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: String?, forKey key: Key) throws

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Data?, forKey key: Key) throws

    /// Encodes the given object weakly for the given key.
    ///
    /// For `Encoder`s that implement this functionality, this will only encode the given object and associate it with the given key if it is encoded unconditionally elsewhere in the payload (either previously or in the future).
    ///
    /// For formats which don't support this feature, the default implementation encodes the given object unconditionally.
    ///
    /// - parameter object: The object to encode.
    /// - parameter key: The key to associate the object with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encodeWeak<T : AnyObject & Encodable>(_ object: T?, forKey key: Key) throws

    /// Stores a keyed encoding container for the given key and returns it.
    ///
    /// - parameter keyType: The key type to use for the container.
    /// - parameter key: The key to encode the container for.
    /// - returns: A new keyed encoding container.
    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey>

    /// Stores an unkeyed encoding container for the given key and returns it.
    ///
    /// - parameter key: The key to encode the container for.
    /// - returns: A new unkeyed encoding container.
    mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer

    /// Stores a new nested container for the default `super` key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// Equivalent to calling `superEncoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    mutating func superEncoder() -> Encoder

    /// Stores a new nested container for the given key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// - parameter key: The key to encode `super` for.
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    mutating func superEncoder(forKey key: Key) -> Encoder
}

// An implementation of _KeyedEncodingContainerBase and _KeyedEncodingContainerBox are given at the bottom of this file.
/// `KeyedEncodingContainer` is a type-erased box for `KeyedEncodingContainerProtocol` types, similar to `AnyCollection` and `AnyHashable`. This is the type which consumers of the API interact with directly.
public struct KeyedEncodingContainer<K : CodingKey> {
    public typealias Key = K

    /// The container for the concrete encoder. The type is _*Base so that it's generic on the key type.
    private var box: _KeyedEncodingContainerBase<Key>

    /// Initializes `self` with the given container.
    ///
    /// - parameter container: The container to hold.
    init<Container : KeyedEncodingContainerProtocol>(_ container: Container) where Container.Key == Key {
        box = _KeyedEncodingContainerBox(container)
    }

    /// The path of coding keys taken to get to this point in encoding.
    /// A `nil` value indicates an unkeyed container.
    public var codingPath: [CodingKey?] {
        return box.codingPath
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Bool?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Int?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Int8?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Int16?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Int32?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Int64?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: UInt?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: UInt8?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: UInt16?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: UInt32?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: UInt64?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Float?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Double?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: String?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode(_ value: Data?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encode<T : Encodable>(_ value: T?, forKey key: Key) throws {
        try box.encode(value, forKey: key)
    }

    /// Encodes the given object weakly for the given key.
    ///
    /// For `Encoder`s that implement this functionality, this will only encode the given object and associate it with the given key if it is encoded unconditionally elsewhere in the payload (either previously or in the future).
    ///
    /// - parameter object: The object to encode.
    /// - parameter key: The key to associate the object with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    public mutating func encodeWeak<T : AnyObject & Encodable>(_ object: T?, forKey key: Key) throws {
        try box.encodeWeak(object, forKey: key)
    }

    /// Stores a keyed encoding container for the given key and returns it.
    ///
    /// - parameter keyType: The key type to use for the container.
    /// - parameter key: The key to encode the container for.
    /// - returns: A new keyed encoding container.
    public mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        return box.nestedContainer(keyedBy: NestedKey.self, forKey: key)
    }

    /// Stores an unkeyed encoding container for the given key and returns it.
    ///
    /// - parameter key: The key to encode the container for.
    /// - returns: A new unkeyed encoding container.
    public mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        return box.nestedUnkeyedContainer(forKey: key)
    }

    /// Stores a new nested container for the default `super` key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// Equivalent to calling `superEncoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    public mutating func superEncoder() -> Encoder {
        return box.superEncoder()
    }

    /// Stores a new nested container for the given key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// - parameter key: The key to encode `super` for.
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    public mutating func superEncoder(forKey key: Key) -> Encoder {
        return box.superEncoder(forKey: key)
    }
}

/// Conformance to `KeyedDecodingContainerProtocol` indicates that a type provides a view into a `Decoder`'s storage and is used to hold the encoded properties of a `Decodable` type in a keyed manner.
///
/// Decoders should provide types conforming to `UnkeyedDecodingContainer` for their format.
public protocol KeyedDecodingContainerProtocol {
    associatedtype Key : CodingKey

    /// The path of coding keys taken to get to this point in decoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// All the keys the `Decoder` has for this container.
    ///
    /// Different keyed containers from the same `Decoder` may return different keys here; it is possible to encode with multiple key types which are not convertible to one another. This should report all keys present which are convertible to the requested type.
    var allKeys: [Key] { get }

    /// Returns whether the `Decoder` contains a value associated with the given key.
    ///
    /// The value associated with the given key may be a null value as appropriate for the data format.
    ///
    /// - parameter key: The key to search for.
    /// - returns: Whether the `Decoder` has an entry for the given key.
    func contains(_ key: Key) -> Bool

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Int.Type, forKey key: Key) throws -> Int

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Float.Type, forKey key: Key) throws -> Float

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Double.Type, forKey key: Key) throws -> Double

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: String.Type, forKey key: Key) throws -> String

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode(_ type: Data.Type, forKey key: Key) throws -> Data

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    func decode<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data?

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T?

    /// Returns the data stored for the given key as represented in a container keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - parameter key: The key that the nested container is associated with.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a keyed container.
    func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey>

    /// Returns the data stored for the given key as represented in an unkeyed container.
    ///
    /// - parameter key: The key that the nested container is associated with.
    /// - returns: An unkeyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not an unkeyed container.
    func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the default `super` key.
    ///
    /// Equivalent to calling `superDecoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the default `super` key, or if the stored value is null.
    func superDecoder() throws -> Decoder

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the given key.
    ///
    /// - parameter key: The key to decode `super` for.
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key, or if the stored value is null.
    func superDecoder(forKey key: Key) throws -> Decoder
}

// An implementation of _KeyedDecodingContainerBase and _KeyedDecodingContainerBox are given at the bottom of this file.
/// `KeyedDecodingContainer` is a type-erased box for `KeyedDecodingContainerProtocol` types, similar to `AnyCollection` and `AnyHashable`. This is the type which consumers of the API interact with directly.
public struct KeyedDecodingContainer<K : CodingKey> : KeyedDecodingContainerProtocol {
    public typealias Key = K

    /// The container for the concrete decoder. The type is _*Base so that it's generic on the key type.
    private var box: _KeyedDecodingContainerBase<Key>

    /// Initializes `self` with the given container.
    ///
    /// - parameter container: The container to hold.
    public init<Container : KeyedDecodingContainerProtocol>(_ container: Container) where Container.Key == Key {
        box = _KeyedDecodingContainerBox(container)
    }

    /// The path of coding keys taken to get to this point in decoding.
    /// A `nil` value indicates an unkeyed container.
    public var codingPath: [CodingKey?] {
        return box.codingPath
    }

    /// All the keys the `Decoder` has for this container.
    ///
    /// Different keyed containers from the same `Decoder` may return different keys here; it is possible to encode with multiple key types which are not convertible to one another. This should report all keys present which are convertible to the requested type.
    public var allKeys: [Key] {
        return box.allKeys
    }

    /// Returns whether the `Decoder` contains a value associated with the given key.
    ///
    /// The value associated with the given key may be a null value as appropriate for the data format.
    ///
    /// - parameter key: The key to search for.
    /// - returns: Whether the `Decoder` has an entry for the given key.
    public func contains(_ key: Key) -> Bool {
        return box.contains(key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        return try box.decodeIfPresent(Bool.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        return try box.decodeIfPresent(Int.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        return try box.decodeIfPresent(Int8.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        return try box.decodeIfPresent(Int16.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        return try box.decodeIfPresent(Int32.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        return try box.decodeIfPresent(Int64.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        return try box.decodeIfPresent(UInt.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        return try box.decodeIfPresent(UInt8.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        return try box.decodeIfPresent(UInt16.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        return try box.decodeIfPresent(UInt32.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        return try box.decodeIfPresent(UInt64.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        return try box.decodeIfPresent(Float.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        return try box.decodeIfPresent(Double.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        return try box.decodeIfPresent(String.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        return try box.decodeIfPresent(Data.self, forKey: key)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    public func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T? {
        return try box.decodeIfPresent(T.self, forKey: key)
    }

    /// Returns the data stored for the given key as represented in a container keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - parameter key: The key that the nested container is associated with.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a keyed container.
    public func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        return try box.nestedContainer(keyedBy: NestedKey.self, forKey: key)
    }

    /// Returns the data stored for the given key as represented in an unkeyed container.
    ///
    /// - parameter key: The key that the nested container is associated with.
    /// - returns: An unkeyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not an unkeyed container.
    public func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        return try box.nestedUnkeyedContainer(forKey: key)
    }

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the default `super` key.
    ///
    /// Equivalent to calling `superDecoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the default `super` key, or if the stored value is null.
    public func superDecoder() throws -> Decoder {
        return try box.superDecoder()
    }

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the given key.
    ///
    /// - parameter key: The key to decode `super` for.
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key, or if the stored value is null.
    public func superDecoder(forKey key: Key) throws -> Decoder {
        return try box.superDecoder(forKey: key)
    }
}

//===----------------------------------------------------------------------===//
// Unkeyed Encoding Containers
//===----------------------------------------------------------------------===//

/// Conformance to `UnkeyedEncodingContainer` indicates that a type provides a view into an `Encoder`'s storage and is used to hold the encoded properties of an `Encodable` type sequentially, without keys.
///
/// Encoders should provide types conforming to `UnkeyedEncodingContainer` for their format.
public protocol UnkeyedEncodingContainer {
    /// The path of coding keys taken to get to this point in encoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode<T : Encodable>(_ value: T?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Bool?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int8?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int16?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int32?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Int64?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt8?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt16?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt32?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: UInt64?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Float?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Double?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: String?) throws

    /// Encodes the given value.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encode(_ value: Data?) throws

    /// Encodes the given object weakly.
    ///
    /// For `Encoder`s that implement this functionality, this will only encode the given object if it is encoded unconditionally elsewhere in the payload (either previously or in the future).
    ///
    /// For formats which don't support this feature, the default implementation encodes the given object unconditionally.
    ///
    /// - parameter object: The object to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    mutating func encodeWeak<T : AnyObject & Encodable>(_ object: T?) throws

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Bool

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int8

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int16

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int32

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int64

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt8

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt16

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt32

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt64

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Float

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Double

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == String

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Data

    /// Encodes the elements of the given sequence.
    ///
    /// - parameter sequence: The sequences whose contents to encode.
    /// - throws: An error if any of the contained values throws an error.
    mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element : Encodable

    /// Encodes a nested container keyed by the given type and returns it.
    ///
    /// - parameter keyType: The key type to use for the container.
    /// - returns: A new keyed encoding container.
    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey>

    /// Encodes an unkeyed encoding container and returns it.
    ///
    /// - returns: A new unkeyed encoding container.
    mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer

    /// Encodes a nested container and returns an `Encoder` instance for encoding `super` into that container.
    ///
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    mutating func superEncoder() -> Encoder
}

/// Conformance to `UnkeyedDecodingContainer` indicates that a type provides a view into a `Decoder`'s storage and is used to hold the encoded properties of a `Decodable` type sequentially, without keys.
///
/// Decoders should provide types conforming to `UnkeyedDecodingContainer` for their format.
public protocol UnkeyedDecodingContainer {
    /// The path of coding keys taken to get to this point in decoding.
    /// A `nil` value indicates an unkeyed container.
    var codingPath: [CodingKey?] { get }

    /// Returns the number of elements (if known) contained within this container.
    var count: Int? { get }

    /// Returns whether there are no more elements left to be decoded in the container.
    var isAtEnd: Bool { get }

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Bool.Type) throws -> Bool

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Int.Type) throws -> Int

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Int8.Type) throws -> Int8

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Int16.Type) throws -> Int16

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Int32.Type) throws -> Int32

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Int64.Type) throws -> Int64

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: UInt.Type) throws -> UInt

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: UInt8.Type) throws -> UInt8

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: UInt16.Type) throws -> UInt16

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: UInt32.Type) throws -> UInt32

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: UInt64.Type) throws -> UInt64

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Float.Type) throws -> Float

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Double.Type) throws -> Double

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: String.Type) throws -> String

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode(_ type: Data.Type) throws -> Data

    /// Decodes a value of the given type.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func decode<T : Decodable>(_ type: T.Type) throws -> T

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Bool.Type) throws -> Bool?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Int.Type) throws -> Int?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Int8.Type) throws -> Int8?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Int16.Type) throws -> Int16?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Int32.Type) throws -> Int32?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Int64.Type) throws -> Int64?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: UInt.Type) throws -> UInt?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: UInt8.Type) throws -> UInt8?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: UInt16.Type) throws -> UInt16?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: UInt32.Type) throws -> UInt32?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: UInt64.Type) throws -> UInt64?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Float.Type) throws -> Float?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Double.Type) throws -> Double?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: String.Type) throws -> String?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent(_ type: Data.Type) throws -> Data?

    /// Decodes a value of the given type, if present.
    ///
    /// This method returns `nil` if the container has no elements left to decode, or if the value is null. The difference between these states can be distinguished by checking `isAtEnd`.
    ///
    /// - parameter type: The type of value to decode.
    /// - returns: A decoded value of the requested type, or `nil` if the value is a null value, or if there are no more elements to decode.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    mutating func decodeIfPresent<T : Decodable>(_ type: T.Type) throws -> T?

    /// Decodes a nested container keyed by the given type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a keyed container.
    mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey>

    /// Decodes an unkeyed nested container.
    ///
    /// - returns: An unkeyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not an unkeyed container.
    mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer

    /// Decodes a nested container and returns a `Decoder` instance for decoding `super` from that container.
    ///
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if the encountered encoded value is null, or of there are no more values to decode.
    mutating func superDecoder() throws -> Decoder
}

//===----------------------------------------------------------------------===//
// Single Value Encoding Containers
//===----------------------------------------------------------------------===//

/// A `SingleValueEncodingContainer` is a container which can support the storage and direct encoding of a single non-keyed value.
public protocol SingleValueEncodingContainer {
    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Bool) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Int) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Int8) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Int16) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Int32) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Int64) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: UInt) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: UInt8) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: UInt16) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: UInt32) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: UInt64) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Float) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Double) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: String) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    mutating func encode(_ value: Data) throws
}

/// A `SingleValueDecodingContainer` is a container which can support the storage and direct decoding of a single non-keyed value.
public protocol SingleValueDecodingContainer {
    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Bool.Type) throws -> Bool

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Int.Type) throws -> Int

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Int8.Type) throws -> Int8

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Int16.Type) throws -> Int16

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Int32.Type) throws -> Int32

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Int64.Type) throws -> Int64

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: UInt.Type) throws -> UInt

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: UInt8.Type) throws -> UInt8

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: UInt16.Type) throws -> UInt16

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: UInt32.Type) throws -> UInt32

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: UInt64.Type) throws -> UInt64

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Float.Type) throws -> Float

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Double.Type) throws -> Double

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: String.Type) throws -> String

    /// Decodes a single value of the given type.
    ///
    /// - parameter type: The type to decode as.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value cannot be converted to the requested type.
    func decode(_ type: Data.Type) throws -> Data
}

//===----------------------------------------------------------------------===//
// User Info
//===----------------------------------------------------------------------===//

/// Represents a user-defined key for providing context for encoding and decoding.
public struct CodingUserInfoKey : RawRepresentable, Equatable, Hashable {
    public typealias RawValue = String

    /// The key's string value.
    public let rawValue: String

    /// Initializes `self` with the given raw value.
    ///
    /// - parameter rawValue: The value of the key.
    public init?(rawValue: String) {
        self.rawValue = rawValue
    }

    /// The key's hash value.
    public var hashValue: Int {
        return self.rawValue.hashValue
    }
}

//===----------------------------------------------------------------------===//
// Errors
//===----------------------------------------------------------------------===//

public let NSCodingPathErrorKey = "NSCodingPath"

fileprivate let foundationBundle = Bundle(for: NSKeyedArchiver.self)

extension CocoaError {
    // TODO: Remove this when real extension is integrated.
    // <rdar://problem/30406648> Foundation overlay should not duplicate CocoaError extensions
    internal static func error(_ code: CocoaError.Code, debugDescription: String, userInfo: [AnyHashable : Any]? = nil, url: URL? = nil) -> Error {
        var info: [AnyHashable : Any] = userInfo ?? [:]
        info["NSDebugDescription"] = debugDescription
        if let url = url {
            info[NSURLErrorKey] = url
        }

        return NSError(domain: NSCocoaErrorDomain, code: code.rawValue, userInfo: info)
    }

    internal static func coderReadCorrupt(at path: [CodingKey?], reason: String) -> Error {
        return CocoaError.error(._coderReadCorrupt, debugDescription: reason, userInfo: [
            NSCodingPathErrorKey: path,
            NSLocalizedDescriptionKey: NSLocalizedString("Err4864",
                                                         tableName: "FoundationErrors",
                                                         bundle: foundationBundle,
                                                         value: "The data couldn’t be read because it isn’t in the correct format.",
                                                         comment: "CocoaError.coderReadCorrupt, the data being read is corrupt or in an unreadable format"),
            NSLocalizedFailureReasonErrorKey: NSLocalizedString("Err4864-C",
                                                                tableName: "FoundationErrors",
                                                                bundle: foundationBundle,
                                                                value: "The data isn’t in the correct format.",
                                                                comment: "CocoaError.coderReadCorrupt, the data being read is corrupt or in an unreadable format"),
        ])
    }

    internal static func coderValueNotFound(at path: [CodingKey?], reason: String) -> Error {
        return CocoaError.error(._coderValueNotFound, debugDescription: reason, userInfo: [
            NSCodingPathErrorKey: path,
            NSLocalizedDescriptionKey: NSLocalizedString("Err4865",
                                                         tableName: "FoundationErrors",
                                                         bundle: foundationBundle,
                                                         value: "The data is missing.",
                                                         comment: "CocoaError.coderValueNotFound, the requested key is not present in the archived data"),
            NSLocalizedFailureReasonErrorKey: NSLocalizedString("Err4865-C",
                                                                tableName: "FoundationErrors",
                                                                bundle: foundationBundle,
                                                                value: "The data is missing.",
                                                                comment: "CocoaError.coderValueNotFound, the requested key is not present in the archived data"),
        ])
    }

    internal static func coderInvalidValue(at path: [CodingKey?], reason: String) -> Error {
        return CocoaError.error(.coderInvalidValue, debugDescription: reason, userInfo: [
            NSCodingPathErrorKey: path,
            // TODO: Set up localized string for this error in Foundation strings.
        ])
    }

    internal static func coderTypeMismatch(at path: [CodingKey?], reason: String) -> Error {
        // This purposefully reuses the localized description from .coderReadCorrupt.
        return CocoaError.error(.coderTypeMismatch, debugDescription: reason, userInfo: [
            NSCodingPathErrorKey: path,
            NSLocalizedDescriptionKey: NSLocalizedString("Err4864",
                                                         tableName: "FoundationErrors",
                                                         bundle: foundationBundle,
                                                         value: "The data couldn’t be read because it isn’t in the correct format.",
                                                         comment: "CocoaError.coderReadCorrupt, the data being read is corrupt or in an unreadable format"),
            NSLocalizedFailureReasonErrorKey: NSLocalizedString("Err4864-C",
                                                                tableName: "FoundationErrors",
                                                                bundle: foundationBundle,
                                                                value: "The data isn’t in the correct format.",
                                                                comment: "CocoaError.coderReadCorrupt, the data being read is corrupt or in an unreadable format"),
        ])
    }
}

//===----------------------------------------------------------------------===//
// Keyed Encoding Container Implementations
//===----------------------------------------------------------------------===//

fileprivate class _KeyedEncodingContainerBase<Key : CodingKey> {
    // These must all be given a concrete implementation in _*Box.
    var codingPath: [CodingKey?] {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Bool?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Int?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Int8?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Int16?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Int32?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Int64?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: UInt?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: UInt8?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: UInt16?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: UInt32?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: UInt64?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Float?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Double?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: String?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode(_ value: Data?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encode<T : Encodable>(_ value: T?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func encodeWeak<T : AnyObject & Encodable>(_ object: T?, forKey key: Key) throws {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func superEncoder() -> Encoder {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }

    func superEncoder(forKey key: Key) -> Encoder {
        fatalError("_KeyedEncodingContainerBase cannot be used directly.")
    }
}

fileprivate final class _KeyedEncodingContainerBox<Concrete : KeyedEncodingContainerProtocol> : _KeyedEncodingContainerBase<Concrete.Key> {
    typealias Key = Concrete.Key

    var concrete: Concrete

    init(_ container: Concrete) {
        concrete = container
    }

    override var codingPath: [CodingKey?] {
        return concrete.codingPath
    }

    override func encode<T : Encodable>(_ value: T?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Bool?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Int?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Int8?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Int16?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Int32?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Int64?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: UInt?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: UInt8?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: UInt16?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: UInt32?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: UInt64?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Float?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Double?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: String?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encode(_ value: Data?, forKey key: Key) throws {
        try concrete.encode(value, forKey: key)
    }

    override func encodeWeak<T : AnyObject & Encodable>(_ object: T?, forKey key: Key) throws {
        try concrete.encodeWeak(object, forKey: key)
    }

    override func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        return concrete.nestedContainer(keyedBy: NestedKey.self, forKey: key)
    }

    override func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        return concrete.nestedUnkeyedContainer(forKey: key)
    }

    override func superEncoder() -> Encoder {
        return concrete.superEncoder()
    }

    override func superEncoder(forKey key: Key) -> Encoder {
        return concrete.superEncoder(forKey: key)
    }
}

fileprivate class _KeyedDecodingContainerBase<Key : CodingKey> {
    var codingPath: [CodingKey?] {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    var allKeys: [Key] {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func contains(_ key: Key) -> Bool {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T? {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func superDecoder() throws -> Decoder {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }

    func superDecoder(forKey key: Key) throws -> Decoder {
        fatalError("_KeyedDecodingContainerBase cannot be used directly.")
    }
}

fileprivate final class _KeyedDecodingContainerBox<Concrete : KeyedDecodingContainerProtocol> : _KeyedDecodingContainerBase<Concrete.Key> {
    typealias Key = Concrete.Key

    var concrete: Concrete

    init(_ container: Concrete) {
        concrete = container
    }

    override var codingPath: [CodingKey?] {
        return concrete.codingPath
    }

    override var allKeys: [Key] {
        return concrete.allKeys
    }

    override func contains(_ key: Key) -> Bool {
        return concrete.contains(key)
    }

    override func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        return try concrete.decodeIfPresent(Bool.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        return try concrete.decodeIfPresent(Int.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        return try concrete.decodeIfPresent(Int8.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        return try concrete.decodeIfPresent(Int16.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        return try concrete.decodeIfPresent(Int32.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        return try concrete.decodeIfPresent(Int64.self, forKey: key)
    }

    override func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        return try concrete.decodeIfPresent(UInt.self, forKey: key)
    }

    override func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        return try concrete.decodeIfPresent(UInt8.self, forKey: key)
    }

    override func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        return try concrete.decodeIfPresent(UInt16.self, forKey: key)
    }

    override func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        return try concrete.decodeIfPresent(UInt32.self, forKey: key)
    }

    override func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        return try concrete.decodeIfPresent(UInt64.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        return try concrete.decodeIfPresent(Float.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        return try concrete.decodeIfPresent(Double.self, forKey: key)
    }

    override func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        return try concrete.decodeIfPresent(String.self, forKey: key)
    }

    override func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        return try concrete.decodeIfPresent(Data.self, forKey: key)
    }

    override func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T? {
        return try concrete.decodeIfPresent(T.self, forKey: key)
    }

    override func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        return try concrete.nestedContainer(keyedBy: NestedKey.self, forKey: key)
    }

    override func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        return try concrete.nestedUnkeyedContainer(forKey: key)
    }

    override func superDecoder() throws -> Decoder {
        return try concrete.superDecoder()
    }

    override func superDecoder(forKey key: Key) throws -> Decoder {
        return try concrete.superDecoder(forKey: key)
    }
}

//===----------------------------------------------------------------------===//
// Primitive and RawRepresentable Extensions
//===----------------------------------------------------------------------===//

extension Bool : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Bool.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Int : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Int8 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int8.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Int16 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int16.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Int32 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int32.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Int64 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int64.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension UInt : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension UInt8 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt8.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension UInt16 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt16.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension UInt32 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt32.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension UInt64 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt64.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Float : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Float.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Double : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Double.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension String : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(String.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Data : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Data.self)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self)
    }
}

extension Date : Codable {
    public init(from decoder: Decoder) throws {
        let timestamp = try decoder.singleValueContainer().decode(Double.self)
        self.init(timeIntervalSince1970: timestamp)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.timeIntervalSince1970)
    }
}

public extension RawRepresentable where RawValue == Bool, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int8, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int16, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int32, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int64, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt8, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt16, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt32, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt64, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Float, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Double, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == String, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Data, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(at: decoder.codingPath, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

//===----------------------------------------------------------------------===//
// Convenience Default Implementations
//===----------------------------------------------------------------------===//

// Default implementation for encodeWeak(_:forKey:) in terms of encode(_:forKey:)
public extension KeyedEncodingContainerProtocol {
    public mutating func encodeWeak<T : AnyObject & Encodable>(_ object: T?, forKey key: Key) throws {
        try encode(object, forKey: key)
    }
}

// Default implementations for decode(_:forKey:) in terms of decodeIfPresent(_:forKey:)
public extension KeyedDecodingContainerProtocol {
    public func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool {
        if let value = try decodeIfPresent(Bool.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Int.Type, forKey key: Key) throws -> Int {
        if let value = try decodeIfPresent(Int.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8 {
        if let value = try decodeIfPresent(Int8.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16 {
        if let value = try decodeIfPresent(Int16.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32 {
        if let value = try decodeIfPresent(Int32.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64 {
        if let value = try decodeIfPresent(Int64.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt {
        if let value = try decodeIfPresent(UInt.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8 {
        if let value = try decodeIfPresent(UInt8.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16 {
        if let value = try decodeIfPresent(UInt16.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32 {
        if let value = try decodeIfPresent(UInt32.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64 {
        if let value = try decodeIfPresent(UInt64.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Float.Type, forKey key: Key) throws -> Float {
        if let value = try decodeIfPresent(Float.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Double.Type, forKey key: Key) throws -> Double {
        if let value = try decodeIfPresent(Double.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: String.Type, forKey key: Key) throws -> String {
        if let value = try decodeIfPresent(String.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode(_ type: Data.Type, forKey key: Key) throws -> Data {
        if let value = try decodeIfPresent(Data.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    public func decode<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T {
        if let value = try decodeIfPresent(T.self, forKey: key) {
            return value
        } else if contains(key) {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var path = codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }
}

// Default implementation of encodeWeak(_:) in terms of encode(_:), and encode(contentsOf:) in terms of encode(_:) loop.
public extension UnkeyedEncodingContainer {
    public mutating func encodeWeak<T : AnyObject & Encodable>(_ object: T?) throws {
        try encode(object)
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Bool {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int8 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int16 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int32 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Int64 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt8 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt16 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt32 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == UInt64 {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Float {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Double {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == String {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element == Data {
        for element in sequence {
            try encode(element)
        }
    }

    public mutating func encode<Sequence : Swift.Sequence>(contentsOf sequence: Sequence) throws where Sequence.Iterator.Element : Encodable {
        for element in sequence {
            try encode(element)
        }
    }
}

// Default implementations for decode(_:) in terms of decodeIfPresent(_:)
public extension UnkeyedDecodingContainer {
    mutating func decode(_ type: Bool.Type) throws -> Bool {
        if let value = try decodeIfPresent(Bool.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Int.Type) throws -> Int {
        if let value = try decodeIfPresent(Int.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Int8.Type) throws -> Int8 {
        if let value = try decodeIfPresent(Int8.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Int16.Type) throws -> Int16 {
        if let value = try decodeIfPresent(Int16.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Int32.Type) throws -> Int32 {
        if let value = try decodeIfPresent(Int32.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Int64.Type) throws -> Int64 {
        if let value = try decodeIfPresent(Int64.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: UInt.Type) throws -> UInt {
        if let value = try decodeIfPresent(UInt.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: UInt8.Type) throws -> UInt8 {
        if let value = try decodeIfPresent(UInt8.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: UInt16.Type) throws -> UInt16 {
        if let value = try decodeIfPresent(UInt16.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: UInt32.Type) throws -> UInt32 {
        if let value = try decodeIfPresent(UInt32.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: UInt64.Type) throws -> UInt64 {
        if let value = try decodeIfPresent(UInt64.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Float.Type) throws -> Float {
        if let value = try decodeIfPresent(Float.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Double.Type) throws -> Double {
        if let value = try decodeIfPresent(Double.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: String.Type) throws -> String {
        if let value = try decodeIfPresent(String.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode(_ type: Data.Type) throws -> Data {
        if let value = try decodeIfPresent(Data.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }

    mutating func decode<T : Decodable>(_ type: T.Type) throws -> T {
        if let value = try decodeIfPresent(T.self) {
            return value
        } else if !isAtEnd {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "Found null value when expecting non-optional type \(type)")
        } else {
            throw CocoaError.coderValueNotFound(at: codingPath, reason: "No remaining elements when expecting non-optional type \(type)")
        }
    }
}
