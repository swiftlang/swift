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
// Plist Encoder
//===----------------------------------------------------------------------===//

/// `PropertyListEncoder` facilitates the encoding of `Encodable` values into property lists.
open class PropertyListEncoder {

    // MARK: - Options

    /// The output format to write the property list data in. Defaults to `.binary`.
    open var outputFormat: PropertyListSerialization.PropertyListFormat = .binary

    /// Contextual user-provided information for use during encoding.
    open var userInfo: [CodingUserInfoKey : Any] = [:]

    /// Options set on the top-level encoder to pass down the encoding hierarchy.
    fileprivate struct _Options {
        let outputFormat: PropertyListSerialization.PropertyListFormat
        let userInfo: [CodingUserInfoKey : Any]
    }

    /// The options set on the top-level encoder.
    fileprivate var options: _Options {
        return _Options(outputFormat: outputFormat, userInfo: userInfo)
    }

    // MARK: - Constructing a Property List Encoder

    /// Initializes `self` with default strategies.
    public init() {}

    // MARK: - Encoding Values

    /// Encodes the given top-level value and returns its property list representation.
    ///
    /// - parameter value: The value to encode.
    /// - returns: A new `Data` value containing the encoded property list data.
    /// - throws: `CocoaError.coderInvalidValue` if a non-comforming floating-point value is encountered during encoding, and the encoding strategy is `.throw`.
    /// - throws: An error if any value throws an error during encoding.
    open func encode<Value : Encodable>(_ value: Value) throws -> Data {
        let encoder = _PlistEncoder(options: self.options)
        try value.encode(to: encoder)

        guard encoder.storage.count > 0 else {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) did not encode any values.")
        }

        let topLevel = encoder.storage.popContainer()
        if topLevel is NSNumber {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as number property list fragment.")
        } else if topLevel is NSString {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as string property list fragment.")
        } else if topLevel is NSDate {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as date property list fragment.")
        }

        return try PropertyListSerialization.data(fromPropertyList: topLevel, format: self.outputFormat, options: 0)
    }
}

// MARK: - _PlistEncoder

fileprivate class _PlistEncoder : Encoder {
    // MARK: Properties

    /// The encoder's storage.
    var storage: _PlistEncodingStorage

    /// Options set on the top-level encoder.
    let options: PropertyListEncoder._Options

    /// The path to the current point in encoding.
    var codingPath: [CodingKey?] = []

    /// Contextual user-provided information for use during encoding.
    var userInfo: [CodingUserInfoKey : Any] {
        return self.options.userInfo
    }

    // MARK: - Initialization

    /// Initializes `self` with the given top-level encoder options.
    init(options: PropertyListEncoder._Options) {
        self.options = options
        self.storage = _PlistEncodingStorage()
    }

    // MARK: - Coding Path Actions

    /// Performs the given closure with the given key pushed onto the end of the current coding path.
    ///
    /// - parameter key: The key to push. May be nil for unkeyed containers.
    /// - parameter work: The work to perform with the key in the path.
    func with(pushedKey key: CodingKey?, _ work: () throws -> ()) rethrows {
        self.codingPath.append(key)
        try work()
        self.codingPath.removeLast()
    }

    /// Asserts that a new container can be requested at this coding path.
    /// `preconditionFailure()`s if one cannot be requested.
    func assertCanRequestNewContainer() {
        // Every time a new value gets encoded, the key it's encoded for is pushed onto the coding path (even if it's a nil key from an unkeyed container).
        // At the same time, every time a container is requested, a new value gets pushed onto the storage stack.
        // If there are more values on the storage stack than on the coding path, it means the value is requesting more than one container, which violates the precondition.

        // This means that anytime something that can request a new container goes onto the stack, we MUST push a key onto the coding path.
        // Things which will not request containers do not need to have the coding path extended for them (but it doesn't matter if it is, because they will not reach here).
        guard self.storage.count == self.codingPath.count else {
            let previousContainerType: String
            if self.storage.containers.last is NSDictionary {
                previousContainerType = "keyed"
            } else if self.storage.containers.last is NSArray {
                previousContainerType = "unkeyed"
            } else {
                previousContainerType = "single value"
            }

            preconditionFailure("Attempt to encode with new container when already encoded with \(previousContainerType) container.")
        }
    }

    // MARK: - Encoder Methods
    func container<Key>(keyedBy: Key.Type) -> KeyedEncodingContainer<Key> {
        assertCanRequestNewContainer()
        let container = self.storage.pushKeyedContainer()
        let wrapper = _PlistKeyedEncodingContainer<Key>(referencing: self, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    func unkeyedContainer() -> UnkeyedEncodingContainer {
        assertCanRequestNewContainer()
        let container = self.storage.pushUnkeyedContainer()
        return _PlistUnkeyedEncodingContainer(referencing: self, wrapping: container)
    }

    func singleValueContainer() -> SingleValueEncodingContainer {
        assertCanRequestNewContainer()
        return self
    }
}

// MARK: - Encoding Storage and Containers

fileprivate struct _PlistEncodingStorage {
    // MARK: Properties

    /// The container stack.
    /// Elements may be any one of the plist types (NSNumber, NSString, NSDate, NSArray, NSDictionary).
    private(set) var containers: [NSObject] = []

    // MARK: - Initialization

    /// Initializes `self` with no containers.
    init() {}

    // MARK: - Modifying the Stack

    var count: Int {
        return self.containers.count
    }

    mutating func pushKeyedContainer() -> NSMutableDictionary {
        let dictionary = NSMutableDictionary()
        self.containers.append(dictionary)
        return dictionary
    }

    mutating func pushUnkeyedContainer() -> NSMutableArray {
        let array = NSMutableArray()
        self.containers.append(array)
        return array
    }

    mutating func push(container: NSObject) {
        self.containers.append(container)
    }

    mutating func popContainer() -> NSObject {
        precondition(self.containers.count > 0, "Empty container stack.")
        return self.containers.popLast()!
    }
}

// MARK: - Encoding Containers

fileprivate struct _PlistKeyedEncodingContainer<K : CodingKey> : KeyedEncodingContainerProtocol {
    typealias Key = K

    // MARK: Properties

    /// A reference to the encoder we're writing to.
    let encoder: _PlistEncoder

    /// A reference to the container we're writing to.
    let container: NSMutableDictionary

    // MARK: - Initialization

    /// Initializes `self` with the given references.
    init(referencing encoder: _PlistEncoder, wrapping container: NSMutableDictionary) {
        self.encoder = encoder
        self.container = container
    }

    // MARK: - KeyedEncodingContainerProtocol Methods

    var codingPath: [CodingKey?] {
        return self.encoder.codingPath
    }

    mutating func encode(_ value: Bool?, forKey key: Key)   throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Int?, forKey key: Key)    throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Int8?, forKey key: Key)   throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Int16?, forKey key: Key)  throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Int32?, forKey key: Key)  throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Int64?, forKey key: Key)  throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: UInt?, forKey key: Key)   throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: UInt8?, forKey key: Key)  throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: UInt16?, forKey key: Key) throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: UInt32?, forKey key: Key) throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: UInt64?, forKey key: Key) throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: String?, forKey key: Key) throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Float?, forKey key: Key)  throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Double?, forKey key: Key) throws { self.container[key.stringValue] = self.encoder.box(value) }
    mutating func encode(_ value: Data?, forKey key: Key)   throws { self.container[key.stringValue] = self.encoder.box(value) }

    mutating func encode<T : Encodable>(_ value: T?, forKey key: Key) throws {
        try self.encoder.with(pushedKey: key) {
            self.container[key.stringValue] = try self.encoder.box(value)
        }
    }

    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        let container = self.encoder.storage.pushKeyedContainer()
        self.container[key.stringValue] = container
        let wrapper = _PlistKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        let container = self.encoder.storage.pushUnkeyedContainer()
        self.container[key.stringValue] = container
        return _PlistUnkeyedEncodingContainer(referencing: self.encoder, wrapping: container)
    }

    mutating func superEncoder() -> Encoder {
        return _PlistReferencingEncoder(referencing: self.encoder, wrapping: self.container, key: "super")
    }

    mutating func superEncoder(forKey key: Key) -> Encoder {
        return _PlistReferencingEncoder(referencing: self.encoder, wrapping: self.container, key: key.stringValue)
    }
}

fileprivate struct _PlistUnkeyedEncodingContainer : UnkeyedEncodingContainer {
    // MARK: Properties

    /// A reference to the encoder we're writing to.
    let encoder: _PlistEncoder

    /// A reference to the container we're writing to.
    let container: NSMutableArray

    // MARK: - Initialization

    /// Initializes `self` with the given references.
    init(referencing encoder: _PlistEncoder, wrapping container: NSMutableArray) {
        self.encoder = encoder
        self.container = container
    }

    // MARK: - UnkeyedEncodingContainer Methods

    var codingPath: [CodingKey?] {
        return self.encoder.codingPath
    }

    mutating func encode(_ value: Bool?)   throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Int?)    throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Int8?)   throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Int16?)  throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Int32?)  throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Int64?)  throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: UInt?)   throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: UInt8?)  throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: UInt16?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: UInt32?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: UInt64?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Float?)  throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Double?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: String?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Data?)   throws { self.container.add(self.encoder.box(value)) }

    mutating func encode<T : Encodable>(_ value: T?) throws {
        try self.encoder.with(pushedKey: nil) {
            self.container.add(try self.encoder.box(value))
        }
    }

    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey> {
        let container = self.encoder.storage.pushKeyedContainer()
        self.container.add(container)
        let wrapper = _PlistKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer {
        let container = self.encoder.storage.pushUnkeyedContainer()
        self.container.add(container)
        return _PlistUnkeyedEncodingContainer(referencing: self.encoder, wrapping: container)
    }

    mutating func superEncoder() -> Encoder {
        return _PlistReferencingEncoder(referencing: self.encoder, wrapping: self.container, at: self.container.count)
    }
}

extension _PlistEncoder : SingleValueEncodingContainer {
    // MARK: Utility

    /// Asserts that a single value can be encoded at the current coding path (i.e. that one has not already been encoded through this container).
    /// `preconditionFailure()`s if one cannot be encoded.
    ///
    /// This is similar to assertCanRequestNewContainer above.
    func assertCanEncodeSingleValue() {
        guard self.storage.count == self.codingPath.count else {
            let previousContainerType: String
            if self.storage.containers.last is NSDictionary {
                previousContainerType = "keyed"
            } else if self.storage.containers.last is NSArray {
                previousContainerType = "unkeyed"
            } else {
                preconditionFailure("Attempt to encode multiple values in a single value container.")
            }

            preconditionFailure("Attempt to encode with new container when already encoded with \(previousContainerType) container.")
        }
    }

    // MARK: - SingleValueEncodingContainer Methods

    func encode(_ value: Bool) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Int) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Int8) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Int16) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Int32) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Int64) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: UInt) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: UInt8) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: UInt16) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: UInt32) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: UInt64) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: String) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Float) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Double) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }

    func encode(_ value: Data) throws {
        assertCanEncodeSingleValue()
        self.storage.push(container: box(value))
    }
}

// MARK: - Concrete Value Representations

extension _PlistEncoder {

    /// Returns the given value boxed in a container appropriate for pushing onto the container stack.
    fileprivate func box(_ value: Bool?)   -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Int?)    -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Int8?)   -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Int16?)  -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Int32?)  -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Int64?)  -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt?)   -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt8?)  -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt16?) -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt32?) -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt64?) -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Float?)  -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: Double?) -> NSObject { return value == nil ? _plistNullNSString : NSNumber(value: value!) }
    fileprivate func box(_ value: String?) -> NSObject { return value == nil ? _plistNullNSString : NSString(string: value!) }
    fileprivate func box(_ value: Data?)   -> NSObject { return value == nil ? _plistNullNSString : NSData(data: value!) }

    fileprivate func box<T : Encodable>(_ value: T?) throws -> NSObject {
        guard let value = value else {
            return _plistNullNSString
        }

        // PropertyListSerialization handles Dates directly.
        if T.self == Date.self {
            return NSDate(timeIntervalSinceReferenceDate: (value as! Date).timeIntervalSinceReferenceDate)
        }

        // The value should request a container from the _PlistEncoder.
        let currentTopContainer = self.storage.containers.last
        try value.encode(to: self)

        // The top container should be a new container.
        guard self.storage.containers.last! !== currentTopContainer else {
            // If the value didn't request a container at all, encode the default container instead.
            return NSDictionary()
        }

        return self.storage.popContainer()
    }
}

// MARK: - _PlistReferencingEncoder

/// _PlistReferencingEncoder is a special subclass of _PlistEncoder which has its own storage, but references the contents of a different encoder.
/// It's used in superEncoder(), which returns a new encoder for encoding a superclass -- the lifetime of the encoder should not escape the scope it's created in, but it doesn't necessarily know when it's done being used (to write to the original container).
fileprivate class _PlistReferencingEncoder : _PlistEncoder {
    // MARK: Reference types.

    /// The type of container we're referencing.
    enum Reference {
        /// Referencing a specific index in an array container.
        case array(NSMutableArray, Int)

        /// Referencing a specific key in a dictionary container.
        case dictionary(NSMutableDictionary, String)
    }

    // MARK: - Properties

    /// The encoder we're referencing.
    let encoder: _PlistEncoder

    /// The container reference itself.
    let reference: Reference

    // MARK: - Initialization

    /// Initializes `self` by referencing the given array container in the given encoder.
    init(referencing encoder: _PlistEncoder, wrapping array: NSMutableArray, at index: Int) {
        self.encoder = encoder
        self.reference = .array(array, index)
        super.init(options: encoder.options)
    }

    /// Initializes `self` by referencing the given dictionary container in the given encoder.
    init(referencing encoder: _PlistEncoder, wrapping dictionary: NSMutableDictionary, key: String) {
        self.encoder = encoder
        self.reference = .dictionary(dictionary, key)
        super.init(options: encoder.options)
    }


    // MARK: - Deinitialization

    // Finalizes `self` by writing the contents of our storage to the referenced encoder's storage.
    deinit {
        // TODO: Ensure self.storage.count == 1, otherwise something went wrong.
        let value = self.storage.popContainer()
        switch self.reference {
        case .array(let array, let index):
            array.insert(value, at: index)

        case .dictionary(let dictionary, let key):
            dictionary[NSString(string: key)] = value
        }
    }
}

//===----------------------------------------------------------------------===//
// Plist Decoder
//===----------------------------------------------------------------------===//

/// `PropertyListDecoder` facilitates the decoding of property list values into semantic `Decodable` types.
open class PropertyListDecoder {
    // MARK: Options

    /// Contextual user-provided information for use during decoding.
    open var userInfo: [CodingUserInfoKey : Any] = [:]

    /// Options set on the top-level encoder to pass down the decoding hierarchy.
    fileprivate struct _Options {
        let userInfo: [CodingUserInfoKey : Any]
    }

    /// The options set on the top-level decoder.
    fileprivate var options: _Options {
        return _Options(userInfo: userInfo)
    }

    // MARK: - Constructing a Property List Decoder

    /// Initializes `self` with default strategies.
    public init() {}

    // MARK: - Decoding Values

    /// Decodes a top-level value of the given type from the given property list representation.
    ///
    /// - parameter type: The type of the value to decode.
    /// - parameter data: The data to decode from.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderReadCorrupt` if values requested from the payload are corrupted, or if the given data is not a valid property list.
    /// - throws: An error if any value throws an error during decoding.
    open func decode<T : Decodable>(_ type: T.Type, from data: Data) throws -> T {
        var format: PropertyListSerialization.PropertyListFormat = .binary
        return try decode(T.self, from: data, format: &format)
    }

    /// Decodes a top-level value of the given type from the given property list representation.
    ///
    /// - parameter type: The type of the value to decode.
    /// - parameter data: The data to decode from.
    /// - parameter format: The parsed property list format.
    /// - returns: A value of the requested type along with the detected format of the property list.
    /// - throws: `CocoaError.coderReadCorrupt` if values requested from the payload are corrupted, or if the given data is not a valid property list.
    /// - throws: An error if any value throws an error during decoding.
    open func decode<T : Decodable>(_ type: T.Type, from data: Data, format: inout PropertyListSerialization.PropertyListFormat) throws -> T {
        let topLevel = try PropertyListSerialization.propertyList(from: data, options: [], format: &format)
        let decoder = _PlistDecoder(referencing: topLevel, options: self.options)
        return try T(from: decoder)
    }
}

// MARK: - _PlistDecoder

fileprivate class _PlistDecoder : Decoder {
    // MARK: Properties

    /// The decoder's storage.
    var storage: _PlistDecodingStorage

    /// Options set on the top-level decoder.
    let options: PropertyListDecoder._Options

    /// The path to the current point in encoding.
    var codingPath: [CodingKey?] = []

    /// Contextual user-provided information for use during encoding.
    var userInfo: [CodingUserInfoKey : Any] {
        return self.options.userInfo
    }

    // MARK: - Initialization

    /// Initializes `self` with the given top-level container and options.
    init(referencing container: Any, options: PropertyListDecoder._Options) {
        self.storage = _PlistDecodingStorage()
        self.storage.push(container: container)
        self.options = options
    }

    // MARK: - Coding Path Actions

    /// Performs the given closure with the given key pushed onto the end of the current coding path.
    ///
    /// - parameter key: The key to push. May be nil for unkeyed containers.
    /// - parameter work: The work to perform with the key in the path.
    func with<T>(pushedKey key: CodingKey?, _ work: () throws -> T) rethrows -> T {
        self.codingPath.append(key)
        let ret: T = try work()
        self.codingPath.removeLast()
        return ret
    }

    // MARK: - Decoder Methods

    func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> {
        guard !(self.storage.topContainer is NSNull) else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get keyed decoding container -- found null value instead.")
        }

        guard let container = self.storage.topContainer as? [String : Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: self.storage.topContainer)
        }

        let wrapper = _PlistKeyedDecodingContainer<Key>(referencing: self, wrapping: container)
        return KeyedDecodingContainer(wrapper)
    }

    func unkeyedContainer() throws -> UnkeyedDecodingContainer {
        guard !(self.storage.topContainer is NSNull) else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get unkeyed decoding container -- found null value instead.")
        }

        guard let container = self.storage.topContainer as? [Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: self.storage.topContainer)
        }

        let wrapper = _PlistUnkeyedDecodingContainer(referencing: self, wrapping: container)
        return wrapper
    }

    func singleValueContainer() throws -> SingleValueDecodingContainer {
        guard !(self.storage.topContainer is NSNull) else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get single value decoding container -- found null value instead.")
        }

        guard !(self.storage.topContainer is [String : Any]) else {
            throw CocoaError.coderTypeMismatch(at: self.codingPath, reason: "Expected single value but keyed container instead.")
        }

        guard !(self.storage.topContainer is [Any]) else {
            throw CocoaError.coderTypeMismatch(at: self.codingPath, reason: "Expected single value but unkeyed container instead.")
        }

        return self
    }
}

// MARK: - Decoding Storage

fileprivate struct _PlistDecodingStorage {
    // MARK: Properties

    /// The container stack.
    /// Elements may be any one of the plist types (NSNumber, Date, String, Array, [String : Any]).
    private(set) var containers: [Any] = []

    // MARK: - Initialization

    /// Initializes `self` with no containers.
    init() {}

    // MARK: - Modifying the Stack

    var count: Int {
        return self.containers.count
    }

    var topContainer: Any {
        precondition(self.containers.count > 0, "Empty container stack.")
        return self.containers.last!
    }

    mutating func push(container: Any) {
        self.containers.append(container)
    }

    mutating func popContainer() {
        precondition(self.containers.count > 0, "Empty container stack.")
        self.containers.removeLast()
    }
}

// MARK: Decoding Containers

fileprivate struct _PlistKeyedDecodingContainer<K : CodingKey> : KeyedDecodingContainerProtocol {
    typealias Key = K

    // MARK: Properties

    /// A reference to the decoder we're reading from.
    let decoder: _PlistDecoder

    /// A reference to the container we're reading from.
    let container: [String : Any]

    // MARK: - Initialization

    /// Initializes `self` by referencing the given decoder and container.
    init(referencing decoder: _PlistDecoder, wrapping container: [String : Any]) {
        self.decoder = decoder
        self.container = container
    }

    // MARK: - KeyedDecodingContainerProtocol Methods

    var codingPath: [CodingKey?] {
        return self.decoder.codingPath
    }

    var allKeys: [Key] {
        return self.container.keys.flatMap { Key(stringValue: $0) }
    }

    func contains(_ key: Key) -> Bool {
        return self.container[key.stringValue] != nil
    }

    func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Bool.self)
        }
    }

    func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Int.self)
        }
    }

    func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Int8.self)
        }
    }

    func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Int16.self)
        }
    }

    func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Int32.self)
        }
    }

    func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Int64.self)
        }
    }

    func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: UInt.self)
        }
    }

    func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: UInt8.self)
        }
    }

    func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: UInt16.self)
        }
    }

    func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: UInt32.self)
        }
    }

    func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: UInt64.self)
        }
    }

    func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Float.self)
        }
    }

    func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Double.self)
        }
    }

    func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: String.self)
        }
    }

    func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: Data.self)
        }
    }

    func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T? {
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(self.container[key.stringValue], as: T.self)
        }
    }

    func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        return try self.decoder.with(pushedKey: key) {
            guard let value = self.container[key.stringValue] else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested keyed container -- no value found for key \"\(key.stringValue)\"")
            }

            guard let container = value as? [String : Any] else {
                throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: value)
            }

            let wrapper = _PlistKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: container)
            return KeyedDecodingContainer(wrapper)
        }
    }

    func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        return try self.decoder.with(pushedKey: key) {
            guard let value = self.container[key.stringValue] else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested unkeyed container -- no value found for key \"\(key.stringValue)\"")
            }

            guard let container = value as? [Any] else {
                throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: value)
            }

            return _PlistUnkeyedDecodingContainer(referencing: self.decoder, wrapping: container)
        }
    }

    func _superDecoder(forKey key: CodingKey) throws -> Decoder {
        return try self.decoder.with(pushedKey: key) {
            guard let value = self.container[key.stringValue] else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get superDecoder() -- no value found for key \"\(key.stringValue)\"")
            }

            return _PlistDecoder(referencing: value, options: self.decoder.options)
        }
    }

    func superDecoder() throws -> Decoder {
        return try _superDecoder(forKey: _PlistDecodingSuperKey())
    }

    func superDecoder(forKey key: Key) throws -> Decoder {
        return try _superDecoder(forKey: key)
    }
}

fileprivate struct _PlistDecodingSuperKey : CodingKey {
    init() {}

    var stringValue: String { return "super" }
    init?(stringValue: String) {
        guard stringValue == "super" else { return nil }
    }

    var intValue: Int? { return nil }
    init?(intValue: Int) {
        return nil
    }
}

fileprivate struct _PlistUnkeyedDecodingContainer : UnkeyedDecodingContainer {
    // MARK: Properties

    /// A reference to the decoder we're reading from.
    let decoder: _PlistDecoder

    /// A reference to the container we're reading from.
    let container: [Any]

    /// The index of the element we're about to decode.
    var currentIndex: Int

    // MARK: - Initialization

    /// Initializes `self` by referencing the given decoder and container.
    init(referencing decoder: _PlistDecoder, wrapping container: [Any]) {
        self.decoder = decoder
        self.container = container
        self.currentIndex = 0
    }

    // MARK: - UnkeyedDecodingContainer Methods

    var codingPath: [CodingKey?] {
        return self.decoder.codingPath
    }

    var count: Int? {
        return self.container.count
    }

    var isAtEnd: Bool {
        return self.currentIndex >= self.count!
    }

    mutating func decodeIfPresent(_ type: Bool.Type) throws -> Bool? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Bool.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Int.Type) throws -> Int? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Int8.Type) throws -> Int8? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int8.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Int16.Type) throws -> Int16? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int16.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Int32.Type) throws -> Int32? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int32.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Int64.Type) throws -> Int64? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int64.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: UInt.Type) throws -> UInt? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: UInt8.Type) throws -> UInt8? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt8.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: UInt16.Type) throws -> UInt16? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt16.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: UInt32.Type) throws -> UInt32? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt32.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: UInt64.Type) throws -> UInt64? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt64.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Float.Type) throws -> Float? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Float.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Double.Type) throws -> Double? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Double.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: String.Type) throws -> String? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: String.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent(_ type: Data.Type) throws -> Data? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Data.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent<T : Decodable>(_ type: T.Type) throws -> T? {
        guard !self.isAtEnd else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: T.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey> {
        return try self.decoder.with(pushedKey: nil) {
            guard !self.isAtEnd else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested keyed container -- unkeyed container is at end.")
            }

            let value = self.container[self.currentIndex]
            guard !(value is NSNull) else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get keyed decoding container -- found null value instead.")
            }

            guard let container = value as? [String : Any] else {
                throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: value)
            }

            self.currentIndex += 1
            let wrapper = _PlistKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: container)
            return KeyedDecodingContainer(wrapper)
        }
    }

    mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer {
        return try self.decoder.with(pushedKey: nil) {
            guard !self.isAtEnd else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested unkeyed container -- unkeyed container is at end.")
            }

            let value = self.container[self.currentIndex]
            guard !(value is NSNull) else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get keyed decoding container -- found null value instead.")
            }

            guard let container = value as? [Any] else {
                throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: value)
            }

            self.currentIndex += 1
            return _PlistUnkeyedDecodingContainer(referencing: self.decoder, wrapping: container)
        }
    }

    mutating func superDecoder() throws -> Decoder {
        return try self.decoder.with(pushedKey: nil) {
            guard !self.isAtEnd else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get superDecoder() -- unkeyed container is at end.")
            }

            let value = self.container[self.currentIndex]
            guard !(value is NSNull) else {
                throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get superDecoder() -- found null value instead.")
            }

            self.currentIndex += 1
            return _PlistDecoder(referencing: value, options: self.decoder.options)
        }
    }
}

extension _PlistDecoder : SingleValueDecodingContainer {
    // MARK: SingleValueDecodingContainer Methods

    // These all unwrap the result, since we couldn't have gotten a single value container if the topContainer was null.
    func decode(_ type: Bool.Type)   throws -> Bool   { return try self.unbox(self.storage.topContainer, as: Bool.self)! }
    func decode(_ type: Int.Type)    throws -> Int    { return try self.unbox(self.storage.topContainer, as: Int.self)! }
    func decode(_ type: Int8.Type)   throws -> Int8   { return try self.unbox(self.storage.topContainer, as: Int8.self)! }
    func decode(_ type: Int16.Type)  throws -> Int16  { return try self.unbox(self.storage.topContainer, as: Int16.self)! }
    func decode(_ type: Int32.Type)  throws -> Int32  { return try self.unbox(self.storage.topContainer, as: Int32.self)! }
    func decode(_ type: Int64.Type)  throws -> Int64  { return try self.unbox(self.storage.topContainer, as: Int64.self)! }
    func decode(_ type: UInt.Type)   throws -> UInt   { return try self.unbox(self.storage.topContainer, as: UInt.self)! }
    func decode(_ type: UInt8.Type)  throws -> UInt8  { return try self.unbox(self.storage.topContainer, as: UInt8.self)! }
    func decode(_ type: UInt16.Type) throws -> UInt16 { return try self.unbox(self.storage.topContainer, as: UInt16.self)! }
    func decode(_ type: UInt32.Type) throws -> UInt32 { return try self.unbox(self.storage.topContainer, as: UInt32.self)! }
    func decode(_ type: UInt64.Type) throws -> UInt64 { return try self.unbox(self.storage.topContainer, as: UInt64.self)! }
    func decode(_ type: Float.Type)  throws -> Float  { return try self.unbox(self.storage.topContainer, as: Float.self)! }
    func decode(_ type: Double.Type) throws -> Double { return try self.unbox(self.storage.topContainer, as: Double.self)! }
    func decode(_ type: String.Type) throws -> String { return try self.unbox(self.storage.topContainer, as: String.self)! }
    func decode(_ type: Data.Type)   throws -> Data   { return try self.unbox(self.storage.topContainer, as: Data.self)! }
}

// MARK: - Concrete Value Representations

extension _PlistDecoder {
    /// Returns the given value unboxed from a container.
    fileprivate func unbox(_ value: Any?, as type: Bool.Type) throws -> Bool? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        if let number = value as? NSNumber {
            // TODO: Add a flag to coerce non-boolean numbers into Bools?
            if number === kCFBooleanTrue as NSNumber {
                return true
            } else if number === kCFBooleanFalse as NSNumber {
                return false
            }

        /* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
        } else if let bool = value as? Bool {
            return bool
        */

        }

        throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
    }

    fileprivate func unbox(_ value: Any?, as type: Int.Type) throws -> Int? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int = number.intValue
        guard NSNumber(value: int) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return int
    }

    fileprivate func unbox(_ value: Any?, as type: Int8.Type) throws -> Int8? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int8 = number.int8Value
        guard NSNumber(value: int8) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return int8
    }

    fileprivate func unbox(_ value: Any?, as type: Int16.Type) throws -> Int16? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int16 = number.int16Value
        guard NSNumber(value: int16) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return int16
    }

    fileprivate func unbox(_ value: Any?, as type: Int32.Type) throws -> Int32? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int32 = number.int32Value
        guard NSNumber(value: int32) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return int32
    }

    fileprivate func unbox(_ value: Any?, as type: Int64.Type) throws -> Int64? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int64 = number.int64Value
        guard NSNumber(value: int64) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return int64
    }

    fileprivate func unbox(_ value: Any?, as type: UInt.Type) throws -> UInt? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint = number.uintValue
        guard NSNumber(value: uint) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return uint
    }

    fileprivate func unbox(_ value: Any?, as type: UInt8.Type) throws -> UInt8? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint8 = number.uint8Value
        guard NSNumber(value: uint8) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return uint8
    }

    fileprivate func unbox(_ value: Any?, as type: UInt16.Type) throws -> UInt16? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint16 = number.uint16Value
        guard NSNumber(value: uint16) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return uint16
    }

    fileprivate func unbox(_ value: Any?, as type: UInt32.Type) throws -> UInt32? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint32 = number.uint32Value
        guard NSNumber(value: uint32) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return uint32
    }

    fileprivate func unbox(_ value: Any?, as type: UInt64.Type) throws -> UInt64? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint64 = number.uint64Value
        guard NSNumber(value: uint64) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return uint64
    }

    fileprivate func unbox(_ value: Any?, as type: Float.Type) throws -> Float? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let float = number.floatValue
        guard NSNumber(value: float) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return float
    }

    func unbox(_ value: Any?, as type: Double.Type) throws -> Double? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let double = number.doubleValue
        guard NSNumber(value: double) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed property list number <\(number)> does not fit in \(type).")
        }

        return double
    }

    func unbox(_ value: Any?, as type: String.Type) throws -> String? {
        guard let value = value else { return nil }

        guard let string = value as? String else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        return string == _plistNull ? nil : string
    }

    func unbox(_ value: Any?, as type: Data.Type) throws -> Data? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let data = value as? Data else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        return data
    }

    func unbox(_ value: Any?, as type: Date.Type) throws -> Date? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        guard let date = value as? Date else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        return date
    }

    func unbox<T : Decodable>(_ value: Any?, as type: T.Type) throws -> T? {
        guard let value = value else { return nil }
        if let string = value as? String, string == _plistNull { return nil }

        let decoded: T
        if T.self == Date.self {
            decoded = (try self.unbox(value, as: Date.self) as! T)
        } else {
            self.storage.push(container: value)
            decoded = try T(from: self)
            self.storage.popContainer()
        }

        return decoded
    }
}

//===----------------------------------------------------------------------===//
// Shared Plist Null Representation
//===----------------------------------------------------------------------===//

// Since plists do not support null values by default, we will encode them as "$null".
fileprivate let _plistNull = "$null"
fileprivate let _plistNullNSString = NSString(string: _plistNull)

//===----------------------------------------------------------------------===//
// CocoaError Extensions
//===----------------------------------------------------------------------===//

extension CocoaError {
    /// Returns an error whose domain is `NSCocoaErrorDomain` and error is `Cocoa.coderTypeMismatch` describing a type mismatch.
    ///
    /// - parameter context: The context in which the error occurred.
    /// - parameter expectation: The type expected to be encountered.
    /// - parameter reality: The value that was encountered instead of the expected type.
    /// - returns: An error appropriate for throwing.
    fileprivate static func typeMismatch(at path: [CodingKey?], expectation: Any.Type, reality: Any) -> Error {
        let message = "Expected to decode \(expectation) but found \(_typeDescription(of: reality)) instead."
        return CocoaError.coderTypeMismatch(at: path, reason: message)
    }

    /// Returns a description of the type of `value` appropriate for an error message.
    ///
    /// - parameter value: The value whose type to describe.
    /// - returns: A string describing `value`.
    /// - precondition: `value` is one of the types below.
    fileprivate static func _typeDescription(of value: Any) -> String {
        if value is NSNumber /* FIXME: If swift-corelibs-foundation isn't updated to use NSNumber, this check will be necessary: || value is Int || value is Double */ {
            return "a number"
        } else if let value = value as? String {
            return value == _plistNull ? "a null value" : "a string/data"
        } else if value is Date {
            return "a date"
        } else if value is [Any] {
            return "an array"
        } else if value is [String : Any] {
            return "a dictionary"
        } else {
            // This should never happen -- we somehow have a non-plist type here.
            preconditionFailure("Invalid storage type \(type(of: value)).")
        }
    }
}
