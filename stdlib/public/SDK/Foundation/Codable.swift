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

/// Conformance to `Codable` indicates that a type can marshal itself into and out of an external representation.
public protocol Codable {
    /// Initializes `self` by decoding from `decoder`.
    ///
    /// - parameter decoder: The decoder to read data from.
    /// - throws: An error if reading from the decoder fails, or if read data is corrupted or otherwise invalid.
    init(from decoder: Decoder) throws

    /// Encodes `self` into the given encoder.
    ///
    /// If `self` fails to encode anything, `encoder` will encode an empty `.default` container in its place.
    ///
    /// - parameter encoder: The encoder to write data to.
    /// - throws: An error if any values are invalid for `encoder`'s format.
    func encode(to encoder: Encoder) throws
}

//===----------------------------------------------------------------------===//
// CodingKey
//===----------------------------------------------------------------------===//

/// Conformance to `CodingKey` indicates that a type can be used as a key for encoding and decoding.
public protocol CodingKey {
    /// The string to use in a named collection (e.g. a string-keyed dictionary).
    var stringValue: String? { get }

    /// Initializes `self` from a string.
    ///
    /// - returns: An instance of `Self` from the given string, or `nil` if the given string does not correspond to any instance of `Self`.
    init?(stringValue: String)

    /// The int to use in an indexed collection (e.g. an int-keyed dictionary).
    var intValue: Int? { get }

    /// Initializes `self` from an integer.
    ///
    /// - returns: An instance of `Self` from the given integer, or `nil` if the given integer does not correspond to any instance of `Self`.
    init?(intValue: Int)
}

//===----------------------------------------------------------------------===//
// Encoder & Decoder
//===----------------------------------------------------------------------===//

/// An `Encoder` is a type which can encode values into a native format for external representation.
public protocol Encoder {
    /// Populates `self` with an encoding container (of `.default` type) and returns it, keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - returns: A new keyed encoding container.
    /// - precondition: May not be called after a previous `self.container(keyedBy:)` call of a different `EncodingContainerType`.
    /// - precondition: May not be called after a value has been encoded through a prior `self.singleValueContainer()` call.
    func container<Key : CodingKey>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key>

    /// Populates `self` with an encoding container of the given type and returns it, keyed by the given key type.
    ///
    /// A default implementation of `Encoder.container(keyedBy:)` calls this method with a container type of `.default`.
    ///
    /// - parameter keyType: The key type to use for the container.
    /// - parameter containerType: The container type to create.
    /// - returns: A new keyed encoding container.
    /// - precondition: May not be called after a previous `self.container(keyedBy:)` call of a different `EncodingContainerType`.
    /// - precondition: May not be called after a value has been encoded through a prior `self.singleValueContainer()` call.
    func container<Key : CodingKey>(keyedBy keyType: Key.Type, type containerType: EncodingContainerType) -> KeyedEncodingContainer<Key>

    /// Returns an encoding container appropriate for holding a single primitive value.
    ///
    /// - returns: A new empty single value container.
    /// - precondition: May not be called after a prior `self.container(keyedBy:)` call.
    /// - precondition: May not be called after a value has been encoded through a previous `self.singleValueContainer()` call.
    func singleValueContainer() -> SingleValueEncodingContainer

    /// The path of coding keys taken to get to this point in encoding.
    var codingKeyContext: [CodingKey] { get }
}

extension Encoder {
    func container<Key : CodingKey>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> {
        return container(keyedBy: Key.self, type: .default)
    }
}

/// An `EncodingContainerType` specifies the type of container an `Encoder` should use to store values.
public enum EncodingContainerType {
    /// The `Encoder`'s preferred container type; equivalent to either `.array` or `.dictionary` as appropriate for the encoder.
    case `default`

    /// Explicitly requests the use of an array to store encoded values.
    case array

    /// Explicitly requests the use of a dictionary to store encoded values.
    case dictionary
}

/// A `Decoder` is a type which can decode values from a native format into in-memory representations.
public protocol Decoder {
    /// Returns the data stored in `self` as represented in a container keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a keyed container.
    func container<Key : CodingKey>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key>

    /// Returns the data stored in `self` as represented in a container appropriate for holding a single primitive value.
    ///
    /// - returns: A single value container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a single value container.
    func singleValueContainer() throws -> SingleValueDecodingContainer

    /// The path of coding keys taken to get to this point in decoding.
    var codingKeyContext: [CodingKey] { get }
}

//===----------------------------------------------------------------------===//
// Keyed Encoding Containers
//===----------------------------------------------------------------------===//

/// `KeyedEncodingContainer` is a generic abstract base class that provides a view into an `Encoder`s storage and is used to hold the encoded properties of a `Codable` type.
///
/// Encoders should provide subclasses of `KeyedEncodingContainer` for their format.
open class KeyedEncodingContainer<Key : CodingKey> {
    /// The path of coding keys taken to get to this point in encoding.
    open var codingKeyContext: [CodingKey] {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode<Value : Codable>(_ value: Value?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Bool?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Int?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Int8?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Int16?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Int32?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Int64?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: UInt?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: UInt8?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: UInt16?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: UInt32?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: UInt64?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Float?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Double?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: String?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given value for the given key.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key to associate the value with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encode(_ value: Data?, forKey key: Key) throws {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Encodes the given object weakly for the given key.
    ///
    /// For `Encoder`s that implement this functionality, this will only encode the given object and associate it with the given key if it encoded unconditionally elsewhere in the archive (either previously or in the future).
    ///
    /// For formats which don't support this feature, the default implementation encodes the given object unconditionally.
    ///
    /// - parameter object: The object to encode.
    /// - parameter key: The key to associate the object with.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func encodeWeak<Object : AnyObject & Codable>(_ object: Object?, forKey key: Key) throws {
        try encode(object, forKey: key)
    }

    /// Stores an encoding container for the given key and returns it.
    ///
    /// - parameter keyType: The key type to use for the container.
    /// - parameter containerType: The container type to create.
    /// - parameter key: The key to encode the container for.
    /// - returns: A new keyed encoding container.
    open func nestedContainer<NestedKey : CodingKey>(keyedBy keyType: NestedKey.Type, type containerType: EncodingContainerType, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Stores a new nested container for the default `super` key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// Equivalent to calling `superEncoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    open func superEncoder() -> Encoder {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Stores a new nested container for the given key and returns a new `Encoder` instance for encoding `super` into that container.
    ///
    /// - parameter key: The key to encode `super` for.
    /// - returns: A new `Encoder` to pass to `super.encode(to:)`.
    /// - precondition: The key must have a `stringValue` or `intValue` appropriate for the encoding container type.
    open func superEncoder(forKey key: Key) -> Encoder {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }
}

/// `KeyedDecodingContainer` is a generic abstract base class that provides a view into an `Decoder`s storage and is used to hold the encoded properties of a `Codable` type.
///
/// Decoders should provide subclasses of `KeyedDecodingContainer` for their format.
open class KeyedDecodingContainer<Key : CodingKey> {
    /// The path of coding keys taken to get to this point in decoding.
    open var codingKeyContext: [CodingKey] {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// All the keys the `Decoder` has for this container.
    ///
    /// Different keyed containers from the same `Decoder` may return different keys here; it is possible to encode with multiple key types which are not convertible to one another. This should report all keys present which are convertible to the requested type.
    open var allKeys: [Key] {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Returns whether the `Decoder` contains a value associated with the given key.
    ///
    /// The value associated with the given key may be a null value as appropriate for the data format.
    ///
    /// - parameter key: The key to search for.
    /// - returns: Whether the `Decoder` has an entry for the given key.
    open func contains(_ key: Key) -> Bool {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool {
        if let value = try decodeIfPresent(Bool.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Int.Type, forKey key: Key) throws -> Int {
        if let value = try decodeIfPresent(Int.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8 {
        if let value = try decodeIfPresent(Int8.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16 {
        if let value = try decodeIfPresent(Int16.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32 {
        if let value = try decodeIfPresent(Int32.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64 {
        if let value = try decodeIfPresent(Int64.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt {
        if let value = try decodeIfPresent(UInt.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8 {
        if let value = try decodeIfPresent(UInt8.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16 {
        if let value = try decodeIfPresent(UInt16.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32 {
        if let value = try decodeIfPresent(UInt32.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64 {
        if let value = try decodeIfPresent(UInt64.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Float.Type, forKey key: Key) throws -> Float {
        if let value = try decodeIfPresent(Float.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Double.Type, forKey key: Key) throws -> Double {
        if let value = try decodeIfPresent(Double.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: String.Type, forKey key: Key) throws -> String {
        if let value = try decodeIfPresent(String.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode(_ type: Data.Type, forKey key: Key) throws -> Data {
        if let value = try decodeIfPresent(Data.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A value of the requested type, if present for the given key and convertible to the requested type.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key or if the value is null.
    open func decode<Value : Codable>(_ type: Value.Type, forKey key: Key) throws -> Value {
        if let value = try decodeIfPresent(Value.self, forKey: key) {
            return value
        } else if contains(key) {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Found null value when expecting non-optional type \(type) for coding key \"\(key)\"")
        } else {
            var context = codingKeyContext
            context.append(key)
            throw CocoaError.coderValueNotFound(in: context, reason: "Key not found when expecting non-optional type \(type) for coding key \"\(key)\"")
        }
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be disambiguated with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Decodes a value of the given type for the given key, if present.
    ///
    /// This method returns `nil` if the container does not have a value associated with `key`, or if the value is null. The difference between these states can be distinguished with a `contains(_:)` call.
    ///
    /// - parameter type: The type of value to decode.
    /// - parameter key: The key that the decoded value is associated with.
    /// - returns: A decoded value of the requested type, or `nil` if the `Decoder` does not have an entry associated with the given key, or if the value is a null value.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered encoded value is not convertible to the requested type.
    open func decodeIfPresent<Value : Codable>(_ type: Value.Type, forKey key: Key) throws -> Value? {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Returns the data stored for the given key as represented in a container keyed by the given key type.
    ///
    /// - parameter type: The key type to use for the container.
    /// - parameter key: The key that the nested container is associated with.
    /// - returns: A keyed decoding container view into `self`.
    /// - throws: `CocoaError.coderTypeMismatch` if the encountered stored value is not a container.
    open func nestedContainer<NestedKey : CodingKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the default `super` key.
    ///
    /// Equivalent to calling `superDecoder(forKey:)` with `Key(stringValue: "super", intValue: 0)`.
    ///
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the default `super` key, or if the stored value is null.
    open func superDecoder() throws -> Decoder {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }

    /// Returns a `Decoder` instance for decoding `super` from the container associated with the given key.
    ///
    /// - parameter key: The key to decode `super` for.
    /// - returns: A new `Decoder` to pass to `super.init(from:)`.
    /// - throws: `CocoaError.coderValueNotFound` if `self` does not have an entry for the given key, or if the stored value is null.
    open func superDecoder(forKey key: Key) throws -> Decoder {
        fatalError("\(#function) must be overriden in subclass implementations", file: #file, line: #line)
    }
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
    func encode(_ value: Bool) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Int) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Int8) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Int16) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Int32) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Int64) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: UInt) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: UInt8) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: UInt16) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: UInt32) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: UInt64) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Float) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Double) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: String) throws

    /// Encodes a single value of the given type.
    ///
    /// - parameter value: The value to encode.
    /// - throws: `CocoaError.coderInvalidValue` if the given value is invalid in the current context for this format.
    /// - precondition: May not be called after a previous `self.encode(_:)` call.
    func encode(_ value: Data) throws
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
// Primitive and RawRepresentable Extensions
//===----------------------------------------------------------------------===//

extension Bool : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Bool.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Int : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Int8 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int8.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Int16 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int16.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Int32 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int32.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Int64 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Int64.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension UInt : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension UInt8 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt8.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension UInt16 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt16.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension UInt32 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt32.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension UInt64 : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(UInt64.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Float : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Float.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Double : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Double.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension String : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(String.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

extension Data : Codable {
    public init(from decoder: Decoder) throws {
        self = try decoder.singleValueContainer().decode(Data.self)
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode( self)
    }
}

public extension RawRepresentable where RawValue == Bool, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int8, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int16, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int32, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Int64, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt8, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt16, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt32, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == UInt64, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Float, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Double, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == String, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

public extension RawRepresentable where RawValue == Data, Self : Codable {
    public init(from decoder: Decoder) throws {
      let decoded = try decoder.singleValueContainer().decode(RawValue.self)
      guard let value = Self(rawValue: decoded) else {
          throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Cannot initialize (Self.self) from invalid (RawValue.self) value (decoded)")
      }

      self = value
    }

    public func encode(to encoder: Encoder) throws {
        try encoder.singleValueContainer().encode(self.rawValue)
    }
}

//===----------------------------------------------------------------------===//
// Errors
//===----------------------------------------------------------------------===//

public let NSCodingKeyContextErrorKey = "NSCodingKeyContext"

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

    internal static func coderReadCorrupt(`in` context: [CodingKey], reason: String) -> Error {
        return CocoaError.error(._coderReadCorrupt, debugDescription: reason, userInfo: [
            NSCodingKeyContextErrorKey: context,
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

    internal static func coderValueNotFound(`in` context: [CodingKey], reason: String) -> Error {
        return CocoaError.error(._coderValueNotFound, debugDescription: reason, userInfo: [
            NSCodingKeyContextErrorKey: context,
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

    internal static func coderInvalidValue(`in` context: [CodingKey], reason: String) -> Error {
        return CocoaError.error(._coderInvalidValue, debugDescription: reason, userInfo: [
            NSCodingKeyContextErrorKey: context,
            // TODO: Set up localized string for this error in Foundation strings.
        ])
    }

    internal static func coderTypeMismatch(`in` context: [CodingKey], reason: String) -> Error {
        // This purposefully reuses the localized description from .coderReadCorrupt.
        return CocoaError.error(._coderTypeMismatch, debugDescription: reason, userInfo: [
            NSCodingKeyContextErrorKey: context,
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

