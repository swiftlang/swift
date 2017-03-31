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
// JSON Encoder
//===----------------------------------------------------------------------===//

/// `JSONEncoder` facilitates the encoding of `Codable` values into JSON.
open class JSONEncoder {
    // MARK: Options

    /// The formatting of the output JSON data.
    public enum OutputFormatting {
        /// Produce JSON compacted by removing whitespace. This is the default formatting.
        case compact

        /// Produce human-readable JSON with indented output.
        case prettyPrinted
    }

    /// The strategy to use for encoding `Date` values.
    public enum DateEncodingStrategy {
        /// Defer to `Date` for choosing an encoding. This is the default strategy.
        case deferredToDate

        /// Encode the `Date` as a UNIX timestamp (as a JSON number).
        case secondsSince1970

        /// Encode the `Date` as UNIX millisecond timestamp (as a JSON number).
        case millisecondsSince1970

        /// Encode the `Date` as an ISO-8601-formatted string (in RFC 3339 format).
        @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
        case iso8601

        /// Encode the `Date` as a string formatted by the given formatter.
        case formatted(DateFormatter)

        /// Encode the `Date` as a custom value encoded by the given closure.
        ///
        /// If the closure fails to encode a value into the given encoder, the encoder will encode an empty automatic container in its place.
        case custom((Date, Encoder) throws -> Void)
    }

    /// The strategy to use for encoding `Data` values.
    public enum DataEncodingStrategy {
        /// Encoded the `Data` as a Base64-encoded string. This is the default strategy.
        case base64Encode

        /// Encode the `Data` as a custom value encoded by the given closure.
        ///
        /// If the closure fails to encode a value into the given encoder, the encoder will encode an empty automatic container in its place.
        case custom((Data, Encoder) throws -> Void)
    }

    /// The strategy to use for non-JSON-conforming floating-point values (IEEE 754 infinity and NaN).
    public enum NonConformingFloatEncodingStrategy {
        /// Throw upon encountering non-conforming values. This is the default strategy.
        case `throw`

        /// Encode the values using the given representation strings.
        case convertToString(positiveInfinity: String, negativeInfinity: String, nan: String)
    }

    /// The output format to produce. Defaults to `.compact`.
    open var outputFormatting: OutputFormatting = .compact

    /// The strategy to use in encoding dates. Defaults to `.deferredToDate`.
    open var dateEncodingStrategy: DateEncodingStrategy = .deferredToDate

    /// The strategy to use in encoding binary data. Defaults to `.base64Encode`.
    open var dataEncodingStrategy: DataEncodingStrategy = .base64Encode

    /// The strategy to use in encoding non-conforming numbers. Defaults to `.throw`.
    open var nonConformingFloatEncodingStrategy: NonConformingFloatEncodingStrategy = .throw

    /// Contextual user-provided information for use during encoding.
    open var userInfo: [CodingUserInfoKey : Any] = [:]

    /// Options set on the top-level encoder to pass down the encoding hierarchy.
    fileprivate struct _Options {
        let dateEncodingStrategy: DateEncodingStrategy
        let dataEncodingStrategy: DataEncodingStrategy
        let nonConformingFloatEncodingStrategy: NonConformingFloatEncodingStrategy
        let userInfo: [CodingUserInfoKey : Any]
    }

    /// The options set on the top-level encoder.
    fileprivate var options: _Options {
        return _Options(dateEncodingStrategy: dateEncodingStrategy,
                        dataEncodingStrategy: dataEncodingStrategy,
                        nonConformingFloatEncodingStrategy: nonConformingFloatEncodingStrategy,
                        userInfo: userInfo)
    }

    // MARK: - Constructing a JSON Encoder

    /// Initializes `self` with default strategies.
    public init() {}

    // MARK: - Encoding Values

    /// Encodes the given top-level value and returns its JSON representation.
    ///
    /// - parameter value: The value to encode.
    /// - returns: A new `Data` value containing the encoded JSON data.
    /// - throws: `CocoaError.coderInvalidValue` if a non-comforming floating-point value is encountered during encoding, and the encoding strategy is `.throw`.
    /// - throws: An error if any value throws an error during encoding.
    open func encode<Value : Encodable>(_ value: Value) throws -> Data {
        let encoder = _JSONEncoder(options: self.options)
        try value.encode(to: encoder)

        guard encoder.storage.count > 0 else {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) did not encode any values.")
        }

        let topLevel = encoder.storage.popContainer()
        if topLevel is NSNull {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as null JSON fragment.")
        } else if topLevel is NSNumber {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as number JSON fragment.")
        } else if topLevel is NSString {
            throw CocoaError.coderInvalidValue(at: [], reason: "Top-level \(Value.self) encoded as string JSON fragment.")
        }

        return try JSONSerialization.data(withJSONObject: topLevel, options: outputFormatting == .prettyPrinted ? .prettyPrinted : [])
    }
}

// MARK: - _JSONEncoder

fileprivate class _JSONEncoder : Encoder {
    // MARK: Properties

    /// The encoder's storage.
    var storage: _JSONEncodingStorage

    /// Options set on the top-level encoder.
    let options: JSONEncoder._Options

    /// The path to the current point in encoding.
    var codingPath: [CodingKey?] = []

    /// Contextual user-provided information for use during encoding.
    var userInfo: [CodingUserInfoKey : Any] {
        return self.options.userInfo
    }

    // MARK: - Initialization

    /// Initializes `self` with the given top-level encoder options.
    init(options: JSONEncoder._Options) {
        self.options = options
        self.storage = _JSONEncodingStorage()
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

    /// Validates that a new container can be requested. `preconditionFailure()`s if one cannot be requested.
    func validateCanRequestContainer() {
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

            preconditionFailure("Attempted to encode with new container when already encoded with \(previousContainerType) container.")
        }
    }

    // MARK: - Encoder Methods
    func container<Key>(keyedBy: Key.Type) -> KeyedEncodingContainer<Key> {
        validateCanRequestContainer()
        let container = self.storage.pushKeyedContainer()
        let wrapper = _JSONKeyedEncodingContainer<Key>(referencing: self, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    func unkeyedContainer() -> UnkeyedEncodingContainer {
        validateCanRequestContainer()
        let container = self.storage.pushUnkeyedContainer()
        return _JSONUnkeyedEncodingContainer(referencing: self, wrapping: container)
    }

    func singleValueContainer() -> SingleValueEncodingContainer {
        validateCanRequestContainer()
        return self
    }
}

// MARK: - Encoding Storage and Containers

fileprivate struct _JSONEncodingStorage {
    // MARK: Properties

    /// The container stack.
    /// Elements may be any one of the JSON types (NSNull, NSNumber, NSString, NSArray, NSDictionary).
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

fileprivate struct _JSONKeyedEncodingContainer<K : CodingKey> : KeyedEncodingContainerProtocol {
    typealias Key = K

    // MARK: Properties

    /// A reference to the encoder we're writing to.
    let encoder: _JSONEncoder

    /// A reference to the container we're writing to.
    let container: NSMutableDictionary

    // MARK: - Initialization

    /// Initializes `self` with the given references.
    init(referencing encoder: _JSONEncoder, wrapping container: NSMutableDictionary) {
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
    mutating func encode(_ value: Float?, forKey key: Key)  throws { self.container[key.stringValue] = try self.encoder.box(value) }
    mutating func encode(_ value: Double?, forKey key: Key) throws { self.container[key.stringValue] = try self.encoder.box(value) }
    mutating func encode(_ value: Data?, forKey key: Key) throws {
        // Since the Data strategy may request a container through a closure, the coding path needs to contain this key.
        try self.encoder.with(pushedKey: key) {
            self.container[key.stringValue] = try self.encoder.box(value)
        }
    }

    mutating func encode<T : Encodable>(_ value: T?, forKey key: Key) throws {
        guard let value = value else {
            self.container[key.stringValue] = NSNull()
            return
        }

        // Respect Date encoding strategy
        if T.self == Date.self {
            // Since the Date strategy may request a container through a closure, the coding path needs to contain this key.
            return try self.encoder.with(pushedKey: nil) {
                self.container[key.stringValue] = try self.encoder.box((value as! Date))
            }
        }

        // The value should request a container from the _JSONEncoder.
        // Since the value is expected to request a container, the coding path needs to contain this key.
        try self.encoder.with(pushedKey: key) {
            try value.encode(to: self.encoder)
        }

        // The top container should be a new container.
        guard self.encoder.storage.containers.last! !== self.container else {
            // If the value didn't request a container at all, encode the default container instead.
            self.container[key.stringValue] = NSDictionary()
            return
        }

        self.container[key.stringValue] = self.encoder.storage.popContainer()
    }

    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        let container = self.encoder.storage.pushKeyedContainer()
        self.container[key.stringValue] = container
        let wrapper = _JSONKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    mutating func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        let container = self.encoder.storage.pushUnkeyedContainer()
        self.container[key.stringValue] = container
        return _JSONUnkeyedEncodingContainer(referencing: self.encoder, wrapping: container)
    }

    mutating func superEncoder() -> Encoder {
        return _JSONReferencingEncoder(referencing: self.encoder, wrapping: self.container, key: "super")
    }

    mutating func superEncoder(forKey key: Key) -> Encoder {
        return _JSONReferencingEncoder(referencing: self.encoder, wrapping: self.container, key: key.stringValue)
    }
}

fileprivate struct _JSONUnkeyedEncodingContainer : UnkeyedEncodingContainer {
    // MARK: Properties

    /// A reference to the encoder we're writing to.
    let encoder: _JSONEncoder

    /// A reference to the container we're writing to.
    let container: NSMutableArray

    // MARK: - Initialization

    /// Initializes `self` with the given references.
    init(referencing encoder: _JSONEncoder, wrapping container: NSMutableArray) {
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
    mutating func encode(_ value: String?) throws { self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Float?)  throws { try self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Double?) throws { try self.container.add(self.encoder.box(value)) }
    mutating func encode(_ value: Data?) throws {
        // Since the Data strategy may request a container through a closure, the coding path needs to contain this key.
        try self.encoder.with(pushedKey: nil) {
            try self.container.add(self.encoder.box(value))
        }
    }

    mutating func encode<T : Encodable>(_ value: T?) throws {
        guard let value = value else {
            self.container.add(NSNull())
            return
        }

        // Respect Date encoding strategy
        if T.self == Date.self {
            // Since the Date strategy may request a container through a closure, the coding path needs to contain this key.
            return try self.encoder.with(pushedKey: nil) {
                try self.container.add(self.encoder.box((value as! Date)))
            }
        }

        // The value should request a container from the _JSONEncoder.
        // Since the value is expected to request a container, the coding path needs to contain this key.
        try self.encoder.with(pushedKey: nil) {
            try value.encode(to: self.encoder)
        }

        // The top container should be a new container.
        guard self.encoder.storage.containers.last! !== self.container else {
            // If the value didn't request a container at all, encode the default container instead.
            self.container.add(NSDictionary())
            return
        }

        self.container.add(self.encoder.storage.popContainer())
    }

    mutating func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey> {
        let container = self.encoder.storage.pushKeyedContainer()
        self.container.add(container)
        let wrapper = _JSONKeyedEncodingContainer<NestedKey>(referencing: self.encoder, wrapping: container)
        return KeyedEncodingContainer(wrapper)
    }

    mutating func nestedUnkeyedContainer() -> UnkeyedEncodingContainer {
        let container = self.encoder.storage.pushUnkeyedContainer()
        self.container.add(container)
        return _JSONUnkeyedEncodingContainer(referencing: self.encoder, wrapping: container)
    }

    mutating func superEncoder() -> Encoder {
        return _JSONReferencingEncoder(referencing: self.encoder, wrapping: self.container, at: self.container.count)
    }
}

extension _JSONEncoder : SingleValueEncodingContainer {
    // MARK: SingleValueEncodingContainer Methods

    func encode(_ value: Bool)   throws { self.storage.push(container: box(value)) }
    func encode(_ value: Int)    throws { self.storage.push(container: box(value)) }
    func encode(_ value: Int8)   throws { self.storage.push(container: box(value)) }
    func encode(_ value: Int16)  throws { self.storage.push(container: box(value)) }
    func encode(_ value: Int32)  throws { self.storage.push(container: box(value)) }
    func encode(_ value: Int64)  throws { self.storage.push(container: box(value)) }
    func encode(_ value: UInt)   throws { self.storage.push(container: box(value)) }
    func encode(_ value: UInt8)  throws { self.storage.push(container: box(value)) }
    func encode(_ value: UInt16) throws { self.storage.push(container: box(value)) }
    func encode(_ value: UInt32) throws { self.storage.push(container: box(value)) }
    func encode(_ value: UInt64) throws { self.storage.push(container: box(value)) }
    func encode(_ value: String) throws { self.storage.push(container: box(value)) }
    func encode(_ value: Float)  throws { try self.storage.push(container: box(value)) }
    func encode(_ value: Double) throws { try self.storage.push(container: box(value)) }
    func encode(_ value: Data) throws {
        // The Data strategy may request a container through a closure.
        // Normally we would have to push a key onto the coding path, but since the single value container doesn't push a container on its own, we're using whichever key has already been pushed onto here for us.
        try self.storage.push(container: box(value))
    }
}

// MARK: - Concrete Value Representations

extension _JSONEncoder {
    /// Returns the given value boxed in a container appropriate for pushing onto the container stack.
    fileprivate func box(_ value: Bool?)    -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: Int?)     -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: Int8?)    -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: Int16?)   -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: Int32?)   -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: Int64?)   -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt?)    -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt8?)   -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt16?)  -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt32?)  -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ value: UInt64?)  -> NSObject { return value == nil ? NSNull() : NSNumber(value: value!) }
    fileprivate func box(_ string: String?) -> NSObject { return string == nil ? NSNull() : NSString(string: string!) }

    fileprivate func box(_ float: Float?) throws -> NSObject {
        guard let float = float else {
            return NSNull()
        }

        guard !float.isInfinite && !float.isNaN else {
            guard case let .convertToString(positiveInfinity: posInfString,
                                            negativeInfinity: negInfString,
                                            nan: nanString) = self.options.nonConformingFloatEncodingStrategy else {
                throw CocoaError.invalidFloatingPointValue(float, at: codingPath)
            }

            if float == Float.infinity {
                return NSString(string: posInfString)
            } else if float == -Float.infinity {
                return NSString(string: negInfString)
            } else {
                return NSString(string: nanString)
            }
        }

        return NSNumber(value: float)
    }

    fileprivate func box(_ double: Double?) throws -> NSObject {
        guard let double = double else {
            return NSNull()
        }

        guard !double.isInfinite && !double.isNaN else {
            guard case let .convertToString(positiveInfinity: posInfString,
                                            negativeInfinity: negInfString,
                                            nan: nanString) = self.options.nonConformingFloatEncodingStrategy else {
                throw CocoaError.invalidFloatingPointValue(double, at: codingPath)
            }

            if double == Double.infinity {
                return NSString(string: posInfString)
            } else if double == -Double.infinity {
                return NSString(string: negInfString)
            } else {
                return NSString(string: nanString)
            }
        }

        return NSNumber(value: double)
    }


    fileprivate func box(_ data: Data?) throws -> NSObject {
        guard let data = data else {
            return NSNull()
        }

        switch self.options.dataEncodingStrategy {
        case .base64Encode:
            return NSString(string: data.base64EncodedString())

        case .custom(let closure):
            let depth = self.storage.count
            try closure(data, self)

            guard self.storage.count > depth else {
                // The closure didn't encode anything. Return the default keyed container.
                return NSDictionary()
            }

            // We can pop because the closure encoded something.
            return self.storage.popContainer()
        }
    }

    fileprivate func box(_ date: Date?) throws -> NSObject {
        guard let date = date else {
            return NSNull()
        }

        switch self.options.dateEncodingStrategy {
        case .deferredToDate:
            // Must be called with a surrounding with(pushedKey:) call.
            try date.encode(to: self)
            return self.storage.popContainer()

        case .secondsSince1970:
            return NSNumber(value: date.timeIntervalSince1970)

        case .millisecondsSince1970:
            return NSNumber(value: 1000.0 * date.timeIntervalSince1970)

        case .iso8601:
            if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
                return NSString(string: _iso8601Formatter.string(from: date))
            } else {
                fatalError("ISO8601DateFormatter is unavailable on this platform.")
            }

        case .formatted(let formatter):
            return NSString(string: formatter.string(from: date))

        case .custom(let closure):
            let depth = self.storage.count
            try closure(date, self)

            guard self.storage.count > depth else {
                // The closure didn't encode anything. Return the default keyed container.
                return NSDictionary()
            }

            // We can pop because the closure encoded something.
            return self.storage.popContainer()
        }
    }
}

// MARK: - _JSONReferencingEncoder

/// _JSONReferencingEncoder is a special subclass of _JSONEncoder which has its own storage, but references the contents of a different encoder.
/// It's used in superEncoder(), which returns a new encoder for encoding a superclass -- the lifetime of the encoder should not escape the scope it's created in, but it doesn't necessarily know when it's done being used (to write to the original container).
fileprivate class _JSONReferencingEncoder : _JSONEncoder {
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
    let encoder: _JSONEncoder

    /// The container reference itself.
    let reference: Reference

    // MARK: - Initialization

    /// Initializes `self` by referencing the given array container in the given encoder.
    init(referencing encoder: _JSONEncoder, wrapping array: NSMutableArray, at index: Int) {
        self.encoder = encoder
        self.reference = .array(array, index)
        super.init(options: encoder.options)
    }

    /// Initializes `self` by referencing the given dictionary container in the given encoder.
    init(referencing encoder: _JSONEncoder, wrapping dictionary: NSMutableDictionary, key: String) {
        self.encoder = encoder
        self.reference = .dictionary(dictionary, key)
        super.init(options: encoder.options)
    }


    // MARK: - Deinitialization

    // Finalizes `self` by writing the contents of our storage to the referenced encoder's storage.
    deinit {
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
// JSON Decoder
//===----------------------------------------------------------------------===//

/// `JSONDecoder` facilitates the decoding of JSON into semantic `Codable` types.
open class JSONDecoder {
    // MARK: Options

    /// The strategy to use for decoding `Date` values.
    public enum DateDecodingStrategy {
        /// Defer to `Date` for decoding. This is the default strategy.
        case deferredToDate

        /// Decode the `Date` as a UNIX timestamp from a JSON number.
        case secondsSince1970

        /// Decode the `Date` as UNIX millisecond timestamp from a JSON number.
        case millisecondsSince1970

        /// Decode the `Date` as an ISO-8601-formatted string (in RFC 3339 format).
        @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
        case iso8601

        /// Decode the `Date` as a string parsed by the given formatter.
        case formatted(DateFormatter)

        /// Decode the `Date` as a custom value decoded by the given closure.
        case custom((_ decoder: Decoder) throws -> Date)
    }

    /// The strategy to use for decoding `Data` values.
    public enum DataDecodingStrategy {
        /// Decode the `Data` from a Base64-encoded string. This is the default strategy.
        case base64Decode

        /// Decode the `Data` as a custom value decoded by the given closure.
        case custom((_ decoder: Decoder) throws -> Data)
    }

    /// The strategy to use for non-JSON-conforming floating-point values (IEEE 754 infinity and NaN).
    public enum NonConformingFloatDecodingStrategy {
        /// Throw upon encountering non-conforming values. This is the default strategy.
        case `throw`

        /// Decode the values from the given representation strings.
        case convertFromString(positiveInfinity: String, negativeInfinity: String, nan: String)
    }

    /// The strategy to use in decoding dates. Defaults to `.deferredToDate`.
    open var dateDecodingStrategy: DateDecodingStrategy = .deferredToDate

    /// The strategy to use in decoding binary data. Defaults to `.base64Decode`.
    open var dataDecodingStrategy: DataDecodingStrategy = .base64Decode

    /// The strategy to use in decoding non-conforming numbers. Defaults to `.throw`.
    open var nonConformingFloatDecodingStrategy: NonConformingFloatDecodingStrategy = .throw

    /// Contextual user-provided information for use during decoding.
    open var userInfo: [CodingUserInfoKey : Any] = [:]

    /// Options set on the top-level encoder to pass down the decoding hierarchy.
    fileprivate struct _Options {
        let dateDecodingStrategy: DateDecodingStrategy
        let dataDecodingStrategy: DataDecodingStrategy
        let nonConformingFloatDecodingStrategy: NonConformingFloatDecodingStrategy
        let userInfo: [CodingUserInfoKey : Any]
    }

    /// The options set on the top-level decoder.
    fileprivate var options: _Options {
        return _Options(dateDecodingStrategy: dateDecodingStrategy,
                        dataDecodingStrategy: dataDecodingStrategy,
                        nonConformingFloatDecodingStrategy: nonConformingFloatDecodingStrategy,
                        userInfo: userInfo)
    }

    // MARK: - Constructing a JSON Encoder

    /// Initializes `self` with default strategies.
    public init() {}

    // MARK: - Decoding Values

    /// Decodes a top-level value of the given type from the given JSON representation.
    ///
    /// - parameter type: The type of the value to decode.
    /// - parameter data: The data to decode from.
    /// - returns: A value of the requested type.
    /// - throws: `CocoaError.coderReadCorrupt` if values requested from the payload are corrupted, or if the given data is not valid JSON.
    /// - throws: An error if any value throws an error during decoding.
    open func decode<Value : Decodable>(_ type: Value.Type, from data: Data) throws -> Value {
        let topLevel = try JSONSerialization.jsonObject(with: data)
        let decoder = _JSONDecoder(referencing: topLevel, options: self.options)
        return try Value(from: decoder)
    }
}

// MARK: - _JSONDecoder

fileprivate class _JSONDecoder : Decoder {
    // MARK: Properties

    /// The decoder's storage.
    var storage: _JSONDecodingStorage

    /// Options set on the top-level decoder.
    let options: JSONDecoder._Options

    /// The path to the current point in encoding.
    var codingPath: [CodingKey?] = []

    /// Contextual user-provided information for use during encoding.
    var userInfo: [CodingUserInfoKey : Any] {
        return self.options.userInfo
    }

    // MARK: - Initialization

    /// Initializes `self` with the given top-level container and options.
    init(referencing container: Any, options: JSONDecoder._Options) {
        self.storage = _JSONDecodingStorage()
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
        guard let container = self.storage.topContainer as? [String : Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: self.storage.topContainer)
        }

        let wrapper = _JSONKeyedDecodingContainer<Key>(referencing: self, wrapping: container)
        return KeyedDecodingContainer(wrapper)
    }

    func unkeyedContainer() throws -> UnkeyedDecodingContainer {
        guard let container = self.storage.topContainer as? [Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: self.storage.topContainer)
        }

        let wrapper = _JSONUnkeyedDecodingContainer(referencing: self, wrapping: container)
        return wrapper
    }

    func singleValueContainer() throws -> SingleValueDecodingContainer {
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

fileprivate struct _JSONDecodingStorage {
    // MARK: Properties

    /// The container stack.
    /// Elements may be any one of the JSON types (NSNull, NSNumber, String, Array, [String : Any]).
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

fileprivate struct _JSONKeyedDecodingContainer<K : CodingKey> : KeyedDecodingContainerProtocol {
    typealias Key = K

    // MARK: Properties

    /// A reference to the decoder we're reading from.
    let decoder: _JSONDecoder

    /// A reference to the container we're reading from.
    let container: [String : Any]

    // MARK: - Initialization

    /// Initializes `self` by referencing the given decoder and container.
    init(referencing decoder: _JSONDecoder, wrapping container: [String : Any]) {
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
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Bool.self)
    }

    func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Int.self)
    }

    func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Int8.self)
    }

    func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Int16.self)
    }

    func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Int32.self)
    }

    func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Int64.self)
    }

    func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: UInt.self)
    }

    func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: UInt8.self)
    }

    func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: UInt16.self)
    }

    func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: UInt32.self)
    }

    func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: UInt64.self)
    }

    func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Float.self)
    }

    func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: Double.self)
    }

    func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        return try self.decoder.unbox(value, as: String.self)
    }

    func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }
        // Since the Data strategy may request a container through a closure, the coding path needs to contain this key.
        return try self.decoder.with(pushedKey: key) {
            return try self.decoder.unbox(value, as: Data.self)
        }
    }

    func decodeIfPresent<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T? {
        guard let value = self.container[key.stringValue] else { return nil }
        guard !(value is NSNull) else { return nil }

        return try self.decoder.with(pushedKey: key) {
            let decoded: T
            if T.self == Date.self {
                decoded = (try self.decoder.unbox(value, as: Date.self) as! T)
            } else {
                self.decoder.storage.push(container: value)
                decoded = try T(from: self.decoder)
                self.decoder.storage.popContainer()
            }

            return decoded
        }
    }

    func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        guard let value = self.container[key.stringValue] else {
            var path = self.codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Cannot get nested keyed container -- no value found for key \"\(key.stringValue)\"")
        }

        guard let container = value as? [String : Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: value)
        }

        let wrapper = _JSONKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: container)
        return KeyedDecodingContainer(wrapper)
    }

    func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
        guard let value = self.container[key.stringValue] else {
            var path = self.codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Cannot get nested unkeyed container -- no value found for key \"\(key.stringValue)\"")
        }

        guard let container = value as? [Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: value)
        }

        return _JSONUnkeyedDecodingContainer(referencing: self.decoder, wrapping: container)
    }

    func _superDecoder(forKey key: CodingKey) throws -> Decoder {
        guard let value = self.container[key.stringValue] else {
            var path = self.codingPath
            path.append(key)
            throw CocoaError.coderValueNotFound(at: path, reason: "Cannot get superDecoder() -- no value found for key \"\(key.stringValue)\"")
        }

        return _JSONDecoder(referencing: value, options: self.decoder.options)
    }

    func superDecoder() throws -> Decoder {
        return try _superDecoder(forKey: _JSONDecodingSuperKey())
    }

    func superDecoder(forKey key: Key) throws -> Decoder {
        return try _superDecoder(forKey: key)
    }
}

fileprivate struct _JSONDecodingSuperKey : CodingKey {
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

fileprivate struct _JSONUnkeyedDecodingContainer : UnkeyedDecodingContainer {
    // MARK: Properties

    /// A reference to the decoder we're reading from.
    let decoder: _JSONDecoder

    /// A reference to the container we're reading from.
    let container: [Any]

    /// The index of the element we're about to decode.
    var currentIndex: Int

    // MARK: - Initialization

    /// Initializes `self` by referencing the given decoder and container.
    init(referencing decoder: _JSONDecoder, wrapping container: [Any]) {
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
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Bool.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Int.Type) throws -> Int? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Int.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Int8.Type) throws -> Int8? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Int8.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Int16.Type) throws -> Int16? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Int16.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Int32.Type) throws -> Int32? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Int32.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Int64.Type) throws -> Int64? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Int64.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: UInt.Type) throws -> UInt? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: UInt.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: UInt8.Type) throws -> UInt8? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: UInt8.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: UInt16.Type) throws -> UInt16? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: UInt16.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: UInt32.Type) throws -> UInt32? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: UInt32.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: UInt64.Type) throws -> UInt64? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: UInt64.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Float.Type) throws -> Float? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Float.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Double.Type) throws -> Double? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: Double.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: String.Type) throws -> String? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
        let decoded = try self.decoder.unbox(value, as: String.self)
        self.currentIndex += 1
        return decoded
    }

    mutating func decodeIfPresent(_ type: Data.Type) throws -> Data? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        // Since the Data strategy may request a container through a closure, the coding path needs to contain this key.
        return try self.decoder.with(pushedKey: nil) {
            // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
            let decoded = try self.decoder.unbox(value, as: Data.self)
            self.currentIndex += 1
            return decoded
        }
    }

    mutating func decodeIfPresent<T : Decodable>(_ type: T.Type) throws -> T? {
        guard !isAtEnd else { return nil }
        let value = self.container[self.currentIndex]
        guard !(value is NSNull) else { return nil }

        return try self.decoder.with(pushedKey: nil) {
            // Don't want to increment currentIndex unless decode succeeds, since we want to be able to attempt to decode the same value again.
            let decoded: T
            if T.self == Date.self {
                decoded = (try self.decoder.unbox(value, as: Date.self) as! T)
            } else {
                self.decoder.storage.push(container: value)
                decoded = try T(from: self.decoder)
                self.decoder.storage.popContainer()
            }

            return decoded
        }
    }

    mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey> {
        guard !isAtEnd else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested keyed container -- unkeyed container is at end.")
        }

        let value = self.container[self.currentIndex]
        guard let container = value as? [String : Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [String : Any].self, reality: value)
        }

        self.currentIndex += 1
        let wrapper = _JSONKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: container)
        return KeyedDecodingContainer(wrapper)
    }

    mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer {
        guard !isAtEnd else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get nested unkeyed container -- unkeyed container is at end.")
        }

        let value = self.container[self.currentIndex]
        guard let container = value as? [Any] else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: [Any].self, reality: value)
        }

        self.currentIndex += 1
        return _JSONUnkeyedDecodingContainer(referencing: self.decoder, wrapping: container)
    }

    mutating func superDecoder() throws -> Decoder {
        guard !isAtEnd else {
            throw CocoaError.coderValueNotFound(at: self.codingPath, reason: "Cannot get superDecoder() -- unkeyed container is at end.")
        }

        let value = self.container[self.currentIndex]
        self.currentIndex += 1
        return _JSONDecoder(referencing: value, options: self.decoder.options)
    }
}

extension _JSONDecoder : SingleValueDecodingContainer {
    // MARK: SingleValueDecodingContainer Methods

    func decode(_ type: Bool.Type)   throws -> Bool   { return try unbox(self.storage.topContainer, as: Bool.self) }
    func decode(_ type: Int.Type)    throws -> Int    { return try unbox(self.storage.topContainer, as: Int.self) }
    func decode(_ type: Int8.Type)   throws -> Int8   { return try unbox(self.storage.topContainer, as: Int8.self) }
    func decode(_ type: Int16.Type)  throws -> Int16  { return try unbox(self.storage.topContainer, as: Int16.self) }
    func decode(_ type: Int32.Type)  throws -> Int32  { return try unbox(self.storage.topContainer, as: Int32.self) }
    func decode(_ type: Int64.Type)  throws -> Int64  { return try unbox(self.storage.topContainer, as: Int64.self) }
    func decode(_ type: UInt.Type)   throws -> UInt   { return try unbox(self.storage.topContainer, as: UInt.self) }
    func decode(_ type: UInt8.Type)  throws -> UInt8  { return try unbox(self.storage.topContainer, as: UInt8.self) }
    func decode(_ type: UInt16.Type) throws -> UInt16 { return try unbox(self.storage.topContainer, as: UInt16.self) }
    func decode(_ type: UInt32.Type) throws -> UInt32 { return try unbox(self.storage.topContainer, as: UInt32.self) }
    func decode(_ type: UInt64.Type) throws -> UInt64 { return try unbox(self.storage.topContainer, as: UInt64.self) }
    func decode(_ type: Float.Type)  throws -> Float  { return try unbox(self.storage.topContainer, as: Float.self) }
    func decode(_ type: Double.Type) throws -> Double { return try unbox(self.storage.topContainer, as: Double.self) }
    func decode(_ type: String.Type) throws -> String { return try unbox(self.storage.topContainer, as: String.self) }
    func decode(_ type: Data.Type)   throws -> Data   { return try unbox(self.storage.topContainer, as: Data.self) }
}

// MARK: - Concrete Value Representations

extension _JSONDecoder {
    /// Returns the given value unboxed from a container.
    fileprivate func unbox(_ value: Any, as type: Bool.Type) throws -> Bool {
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

    fileprivate func unbox(_ value: Any, as type: Int.Type) throws -> Int {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int = number.intValue
        guard NSNumber(value: int) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return int
    }

    fileprivate func unbox(_ value: Any, as type: Int8.Type) throws -> Int8 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int8 = number.int8Value
        guard NSNumber(value: int8) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return int8
    }

    fileprivate func unbox(_ value: Any, as type: Int16.Type) throws -> Int16 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int16 = number.int16Value
        guard NSNumber(value: int16) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return int16
    }

    fileprivate func unbox(_ value: Any, as type: Int32.Type) throws -> Int32 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int32 = number.int32Value
        guard NSNumber(value: int32) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return int32
    }

    fileprivate func unbox(_ value: Any, as type: Int64.Type) throws -> Int64 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let int64 = number.int64Value
        guard NSNumber(value: int64) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return int64
    }

    fileprivate func unbox(_ value: Any, as type: UInt.Type) throws -> UInt {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint = number.uintValue
        guard NSNumber(value: uint) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return uint
    }

    fileprivate func unbox(_ value: Any, as type: UInt8.Type) throws -> UInt8 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint8 = number.uint8Value
        guard NSNumber(value: uint8) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return uint8
    }

    fileprivate func unbox(_ value: Any, as type: UInt16.Type) throws -> UInt16 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint16 = number.uint16Value
        guard NSNumber(value: uint16) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return uint16
    }

    fileprivate func unbox(_ value: Any, as type: UInt32.Type) throws -> UInt32 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint32 = number.uint32Value
        guard NSNumber(value: uint32) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return uint32
    }

    fileprivate func unbox(_ value: Any, as type: UInt64.Type) throws -> UInt64 {
        guard let number = value as? NSNumber else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        let uint64 = number.uint64Value
        guard NSNumber(value: uint64) == number else {
            throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number <\(number)> does not fit in \(type).")
        }

        return uint64
    }

    fileprivate func unbox(_ value: Any, as type: Float.Type) throws -> Float {
        if let number = value as? NSNumber {
            // We are willing to return a Float by losing precision:
            // * If the original value was integral,
            //   * and the integral value was > Float.greatestFiniteMagnitude, we will fail
            //   * and the integral value was <= Float.greatestFiniteMagnitude, we are willing to lose precision past 2^24
            // * If it was a Float, you will get back the precise value
            // * If it was a Double or Decimal, you will get back the nearest approximation if it will fit
            let double = number.doubleValue
            guard abs(double) <= Double(Float.greatestFiniteMagnitude) else {
                throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Parsed JSON number \(number) does not fit in \(type).")
            }

            return Float(double)

        /* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
        } else if let double = value as? Double {
            if abs(double) <= Double(Float.max) {
                return Float(double)
            }

            overflow = true
        } else if let int = value as? Int {
            if let float = Float(exactly: int) {
                return float
            }

            overflow = true
        */

        } else if let string = value as? String,
            case .convertFromString(let posInfString, let negInfString, let nanString) = self.options.nonConformingFloatDecodingStrategy {
            if string == posInfString {
                return Float.infinity
            } else if string == negInfString {
                return -Float.infinity
            } else if string == nanString {
                return Float.nan
            }
        }

        throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
    }

    func unbox(_ value: Any, as type: Double.Type) throws -> Double {
        if let number = value as? NSNumber {
            // We are always willing to return the number as a Double:
            // * If the original value was integral, it is guaranteed to fit in a Double; we are willing to lose precision past 2^53 if you encoded a UInt64 but requested a Double
            // * If it was a Float or Double, you will get back the precise value
            // * If it was Decimal, you will get back the nearest approximation
            return number.doubleValue

        /* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
        } else if let double = value as? Double {
            return double
        } else if let int = value as? Int {
            if let double = Double(exactly: int) {
                return double
            }

            overflow = true
        */

        } else if let string = value as? String,
            case .convertFromString(let posInfString, let negInfString, let nanString) = self.options.nonConformingFloatDecodingStrategy {
            if string == posInfString {
                return Double.infinity
            } else if string == negInfString {
                return -Double.infinity
            } else if string == nanString {
                return Double.nan
            }
        }

        throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
    }

    func unbox(_ value: Any, as type: String.Type) throws -> String {
        guard let string = value as? String else {
            throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
        }

        return string
    }

    func unbox(_ value: Any, as type: Data.Type) throws -> Data {
        switch self.options.dataDecodingStrategy {
        case .base64Decode:
            guard let string = value as? String else {
                throw CocoaError.typeMismatch(at: self.codingPath, expectation: type, reality: value)
            }

            guard let data = Data(base64Encoded: string) else {
                throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Encountered Data is not valid Base64.")
            }

            return data

        case .custom(let closure):
            self.storage.push(container: value)
            let data = try closure(self)
            self.storage.popContainer()
            return data
        }
    }

    func unbox(_ value: Any, as type: Date.Type) throws -> Date {
        switch self.options.dateDecodingStrategy {
        case .deferredToDate:
            self.storage.push(container: value)
            let date = try Date(from: self)
            self.storage.popContainer()
            return date

        case .secondsSince1970:
            let double = try unbox(value, as: Double.self)
            return Date(timeIntervalSince1970: double)

        case .millisecondsSince1970:
            let double = try unbox(value, as: Double.self)
            return Date(timeIntervalSince1970: double / 1000.0)

        case .iso8601:
            if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
                let string = try unbox(value, as: String.self)
                guard let date = _iso8601Formatter.date(from: string) else {
                    throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Expected date string to be ISO8601-formatted.")
                }

                return date
            } else {
                fatalError("ISO8601DateFormatter is unavailable on this platform.")
            }

        case .formatted(let formatter):
            let string = try unbox(value, as: String.self)
            guard let date = formatter.date(from: string) else {
                throw CocoaError.coderReadCorrupt(at: self.codingPath, reason: "Date string does not match format expected by formatter.")
            }

            return date

        case .custom(let closure):
            self.storage.push(container: value)
            let date = try closure(self)
            self.storage.popContainer()
            return date
        }
    }
}

//===----------------------------------------------------------------------===//
// Shared ISO8601 Date Formatter
//===----------------------------------------------------------------------===//

// NOTE: This value is implicitly lazy and _must_ be lazy. We're compiled against the latest SDK (w/ ISO8601DateFormatter), but linked against whichever Foundation the user has. ISO8601DateFormatter might not exist, so we better not hit this code path on an older OS.
@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
fileprivate var _iso8601Formatter: ISO8601DateFormatter = {
    let formatter = ISO8601DateFormatter()
    formatter.formatOptions = .withInternetDateTime
    return formatter
}()

//===----------------------------------------------------------------------===//
// CocoaError Extensions
//===----------------------------------------------------------------------===//

extension CocoaError {
    /// Returns an error whose domain is `NSCocoaErrorDomain` and error is `CocoaError.coderInvalidValue`.
    ///
    /// - parameter value: The value that was invalid to encode.
    /// - parameter context: The context the value was attempted to be encoded in.
    /// - returns: An `Error` with the appropriate localized description, context, and debug description.
    fileprivate static func invalidFloatingPointValue<T : FloatingPoint>(_ value: T, at path: [CodingKey?]) -> Error {
        let valueDescription: String
        if value == T.infinity {
            valueDescription = "\(T.self).infinity"
        } else if value == -T.infinity {
            valueDescription = "-\(T.self).infinity"
        } else {
            valueDescription = "\(T.self).nan"
        }

        let message = "Unable to encode \(valueDescription) directly in JSON. Use JSONEncoder.NonConformingFloatEncodingStrategy.convertToString to encode the value instead."
        return CocoaError.coderInvalidValue(at: path, reason: message)
    }

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
        if value is NSNull {
            return "a null value"
        } else if value is NSNumber /* FIXME: If swift-corelibs-foundation isn't updated to use NSNumber, this check will be necessary: || value is Int || value is Double */ {
            return "a number"
        } else if value is String {
            return "a string/data"
        } else if value is [Any] {
            return "an array"
        } else if value is [String : Any] {
            return "a dictionary"
        } else {
            // This should never happen -- we somehow have a non-JSON type here.
            preconditionFailure("Invalid storage type \(type(of: value)).")
        }
    }
}
