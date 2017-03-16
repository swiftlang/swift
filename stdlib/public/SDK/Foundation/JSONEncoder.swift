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
    // MARK: - Options

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

    /// A container for strategies that can be passed around down the encoding hierarchy.
    fileprivate struct _Strategies {
        let dateEncodingStrategy: DateEncodingStrategy
        let dataEncodingStrategy: DataEncodingStrategy
        let nonConformingFloatEncodingStrategy: NonConformingFloatEncodingStrategy
    }

    /// The strategies set on the top-level encoder.
    fileprivate var strategies: _Strategies {
        return _Strategies(dateEncodingStrategy: dateEncodingStrategy,
                           dataEncodingStrategy: dataEncodingStrategy,
                           nonConformingFloatEncodingStrategy: nonConformingFloatEncodingStrategy)
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
    open func encode<Value : Codable>(_ value: Value) throws -> Data {
        let encoder = _JSONEncoder(strategies: self.strategies)
        try value.encode(to: encoder)

        let storage = encoder.storage
        if let topLevel = storage.value {
            if topLevel is NSNull {
                throw CocoaError.coderInvalidValue(in: [], reason: "Top-level \(Value.self) encoded as null JSON fragment.")
            } else if topLevel is NSNumber {
                throw CocoaError.coderInvalidValue(in: [], reason: "Top-level \(Value.self) encoded as number JSON fragment.")
            } else if topLevel is NSString {
                throw CocoaError.coderInvalidValue(in: [], reason: "Top-level \(Value.self) encoded as string JSON fragment.")
            }

            return try JSONSerialization.data(withJSONObject: topLevel, options: outputFormatting == .prettyPrinted ? .prettyPrinted : [])
        } else {
            throw CocoaError.coderInvalidValue(in: [], reason: "Top-level \(Value.self) did not encode any values.")
        }
    }
}

/// A container for storing JSON values for encoding in a type-safe manner.
fileprivate enum _JSONEncoderStorage {
    /// No value has been encoded yet.
    case empty

    /// A nil value was encoded.
    case null

    /// One of the number primitives.
    case number(NSNumber)

    /// A string or encoded data.
    case string(String)

    /// An array container.
    case array(NSMutableArray)

    /// A dictionary container.
    case dictionary(NSMutableDictionary)

    /// A reference to the storage of a parent encoder.
    case reference(_JSONEncoder, CodingKey)

    // MARK: - Storage Access

    /// Returns the value stored in the storage.
    ///
    /// If the storage is a reference to a different container, follows the reference through. Returns `nil` if the reference is a forward reference (the referenced encoder has nothing stored for this key yet -- we created a a reference but haven't stored through it yet).
    var value: Any? {
        switch self {
        case .empty: return nil
        case .null: return NSNull()
        case .number(let number): return number
        case .string(let string): return string
        case .array(let array): return array
        case .dictionary(let dictionary): return dictionary

        case .reference(let encoder, let key):
            switch encoder.storage {
            case .array(let refArray):
                guard let index = key.intValue else {
                    // This should never happen -- we created a reference to an array with a non-integer key.
                    fatalError("Unable to access array reference with non-integer key.")
                }

                guard (0 ..< refArray.count).contains(index) else {
                    // The nil return here is deliberate. This is purposefully checked against in store(deferred:forKey:) and _JSONEncoder.container(keyedBy:type:) to test if something has been written through to this reference or not.
                    // If the referenced key does not exist yet, it hasn't been written to, so this is returning nil.
                    return nil
                }

                return refArray[index]

            case .dictionary(let refDictionary):
                guard let name = _extractString(fromKey: key) else {
                    // This should never happen -- we created a reference to a dictionary with an empty key.
                    fatalError("Unable to access dictionary reference with empty key.")
                }

                return refDictionary[name]

            default:
                // This should never happen -- we somehow created a reference to something that is neither an array nor a dictionary.
                fatalError("Unable to access non-container reference by key.")
            }
        }
    }

    // MARK: - Non-Keyed Storage Mutation

    /// Stores a value in `self` if it is empty.
    ///
    /// - parameter value: The value to store.
    /// - precondition: `self` is currently `.empty`.
    mutating func store(_ value: Any?) {
        let invalidTypeName: String
        switch self {
        case .empty:
            guard let value = value else {
                return
            }

            if value is NSNull {
                self = .null
            } else if let number = value as? NSNumber {
                self = .number(number)
            } else if let string = value as? String {
                self = .string(string)
            } else if let array = value as? NSMutableArray {
                self = .array(array)
            } else if let dictionary = value as? NSMutableDictionary {
                self = .dictionary(dictionary)
            } else {
                // This should never happen -- we called into here ourselves with an unrelated type.
                fatalError("Value of type \(type(of: value)) not valid for direct storage.")
            }

            // Want to visually call out this return here; .empty shouldn't fail below.
            return

        case .null: invalidTypeName = "null"
        case .number: invalidTypeName = "number"
        case .string: invalidTypeName = "string"
        case .array: invalidTypeName = "array"
        case .dictionary: invalidTypeName = "dictionary"
        case .reference: invalidTypeName = "reference"
        }

        preconditionFailure("Invalid reencode -- attempt to store \(type(of: value)) when already stored as \(invalidTypeName).")
    }

    // MARK: - Keyed Storage Mutation

    /// Stores a value for the given key in `self`.
    ///
    /// - parameter value: A closure which evaluates to the value being stored.
    /// - parameter key: The key to store the value for.
    /// - precondition: `self` is `.array`, `.dictionary`, or `.reference`.
    mutating func store(_ value: @autoclosure() throws -> Any, forKey key: CodingKey) rethrows {
        try store(deferred: value, forKey: key)
    }

    /// Stores a value for the given key in `self`.
    ///
    /// - parameter encoding: A closure which evaluates to the value being stored.
    /// - parameter key: The key to store the value for.
    /// - precondition: `self` is `.array`, `.dictionary`, or `.reference`.
    mutating func store(deferred encoding: () throws -> Any, forKey key: CodingKey) rethrows {
        switch self {
        case .array(let array):
            guard let index = key.intValue else {
                preconditionFailure("Unable to access array via key with no intValue: \(key)")
            }

            guard index >= 0 else {
                preconditionFailure("Array index may not be negative: \(index)")
            }

            // Want to evaluate this value before potentially modifying the array, in case it fails.
            let evaluated = try encoding()

            while array.count > index {
                // FIXME: When NSMutableArray has more efficient batch insertion, use here.
                array.add(NSNull())
            }

            array[index] = evaluated

        case .dictionary(let dictionary):
            guard let name = _extractString(fromKey: key) else {
                preconditionFailure("Unable to access dictionary via empty key: \(key)")
            }

            dictionary[name] = try encoding()

        case .reference(_, _):
            // We are in a child encoder, referencing a parent. We need to pull a container out of this reference, and store the given value for the given key in _that_ container.
            if let container = self.value {
                // The encoder already has an entry for the reference key.
                let innerStorage: _JSONEncoderStorage
                if let array = container as? NSMutableArray {
                    innerStorage = .array(array)
                } else if let dictionary = container as? NSMutableDictionary {
                    innerStorage = .dictionary(dictionary)
                } else {
                    // This should never happen -- we have a reference to something that is not a container.
                    fatalError("Unable to store value by key in non-container reference.")
                }

                try innerStorage.store(deferred: encoding, forKey: key)
            } else {
                // This should never happen -- we have a reference to a container which doesn't exist.
                fatalError("Unable to store value by key in nil reference.")
            }

        default:
            preconditionFailure("Invalid encode -- unable to store value by key in non-container storage.")
        }
    }
}

// MARK: - Inner JSON Encoder

fileprivate class _JSONEncoder : Encoder, SingleValueEncodingContainer {
    /// The value storage for this hierarchy in encoding.
    var storage: _JSONEncoderStorage

    /// The path of coding keys taken to get to this point in encoding.
    let codingKeyContext: [CodingKey]

    /// Strategies to use in converting values for encoding.
    let strategies: JSONEncoder._Strategies

    /// Initializes `self` with the given strategies.
    ///
    /// - parameter storage: A preexisting value to store if not `.empty`.
    /// - parameter strategies: The strategies to use in encoding.
    /// - parameter context: The encoding context we're working in.
    init(storage: _JSONEncoderStorage = .empty, strategies: JSONEncoder._Strategies, context: [CodingKey] = []) {
        self.storage = storage
        self.strategies = strategies
        self.codingKeyContext = context
    }

    // MARK: - Encoder Methods

    func container<Key : CodingKey>(keyedBy keyType: Key.Type, type containerType: EncodingContainerType) -> KeyedEncodingContainer<Key> {
        var invalidTypeName: String? = nil
        switch storage {
        case .empty:
            switch containerType {
                case .array: storage.store(NSMutableArray())
                default: storage.store(NSMutableDictionary())
            }

            case .array(_):
                guard containerType != .dictionary else {
                    preconditionFailure("Cannot get dictionary keyed view into array container.")
                }

            case .dictionary(_):
                guard containerType != .array else {
                    preconditionFailure("Cannot get array keyed view into dictionary container.")
                }

            case .reference(let encoder, let key):
                // We're in a nested encoder, referencing a parent.
                // Need to check the type of the contained data. If it doesn't exist then we can create a new container; if not, need to check for validity.
                let reference = storage.value
                if reference == nil {
                    // Nothing was stored through this reference yet; we can overwrite.
                    // This is the same as the .empty case above.
                    switch containerType {
                        case .array: encoder.storage.store(NSMutableArray(), forKey: key)
                        default: encoder.storage.store(NSMutableDictionary(), forKey: key)
                    }
                } else if reference is NSNull,
                case .array(let array) = encoder.storage {
                    // The value in the storage is an `NSNull`, but we expected it to be empty. Normally, we don't allow overwriting values in this way -- if a null value has already been encoded, we fail.
                    // However, it is possible that we are referencing an array which has since had a value encoded at a discontiguous index, padding the intermediate indices with `NSNull`s (e.g. encode via an array, get a `superEncoder()` without encoding yet, encode past the super index, then encode super and reach here).
                    // We will allow overwriting `NSNull`s here only for arrays, since we cannot know whether the user has legitimately encoded or not.
                    guard let index = key.intValue else {
                        preconditionFailure("Unable to access array via key with no intValue: \(key)")
                    }

                    // Overwrite manually since `encoder.storage.store(_:forKey:)` will error on overwrite.
                    switch containerType {
                        case .array: array[index] = NSMutableArray()
                        default: array[index] = NSMutableDictionary()
                    }
                } else {
                    // We're referencing a container -- ensure the type matches up with the requested container.
                    // This is the same as the .array and .dictionary cases above.
                    if reference is NSMutableArray {
                        guard containerType != .dictionary else {
                            preconditionFailure("Cannot get dictionary keyed view into array container reference.")
                        }
                    } else if reference is NSMutableDictionary {
                        guard containerType != .array else {
                            preconditionFailure("Cannot get array keyed view into dictionary container reference.")
                        }
                    } else {
                        // This should never happen -- we somehow created a reference to something that is neither an array nor a dictionary.
                        fatalError("Unable to access non-container reference by key.")
                    }
                }

                case .null: invalidTypeName = "null"
                case .number(_): invalidTypeName = "number"
                case .string(_): invalidTypeName = "string"
        }

        if let invalidTypeName = invalidTypeName {
            preconditionFailure("Invalid reencode -- attempt to store container when already encoded as \(invalidTypeName).")
        } else {
            return _JSONKeyedEncodingContainer<Key>(referencing: self)
        }
    }

    func singleValueContainer() -> SingleValueEncodingContainer {
        return self
    }

    // MARK: - SingleValueEncodingContainer Methods

    func encode(_ value: Bool) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Int) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Int8) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Int16) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Int32) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Int64) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: UInt) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: UInt8) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: UInt16) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: UInt32) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: UInt64) throws {
        storage.store(NSNumber(value: value))
    }

    func encode(_ value: Float) throws {
        if let boxed = _box(value, strategy: strategies.nonConformingFloatEncodingStrategy) {
            storage.store(boxed)
        } else {
            throw CocoaError.invalidFloatingPointValue(value, in: codingKeyContext)
        }
    }

    func encode(_ value: Double) throws {
        if let boxed = _box(value, strategy: strategies.nonConformingFloatEncodingStrategy) {
            storage.store(boxed)
        } else {
            throw CocoaError.invalidFloatingPointValue(value, in: codingKeyContext)
        }
    }

    func encode(_ value: String) throws {
        storage.store(value)
    }

    func encode(_ value: Data) throws {
        switch strategies.dataEncodingStrategy {
        case .base64Encode:
            storage.store(value.base64EncodedString())

        case .custom(let closure):
            try closure(value, self)
        }
    }
}

// MARK: - JSON Keyed Encoding Container

// NOTE: This value is implicitly lazy and _must_ be lazy. We're compiled against the latest SDK (w/ ISO8601DateFormatter), but linked against whichever Foundation the user has. ISO8601DateFormatter might not exist, so we better not hit this code path on an older OS.
@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
fileprivate var _iso8601Formatter: ISO8601DateFormatter = {
    let formatter = ISO8601DateFormatter()
    formatter.formatOptions = .withInternetDateTime
    return formatter
}()

fileprivate class _JSONKeyedEncodingContainer<Key : CodingKey> : KeyedEncodingContainer<Key> {
    /// A reference to the concrete encoder.
    let encoder: _JSONEncoder

    /// Initializes `self` by referencing the given encoder.
    ///
    /// - parameter encoder: The existing encoder to reference.
    init(referencing encoder: _JSONEncoder) {
        self.encoder = encoder
    }

    // MARK - Keyed Value Encoding

    /// Encodes the given value in a new `Encoder` and returns the encoded value.
    ///
    /// - parameter value: The value to encode.
    /// - parameter key: The key we will be storing the value for. Pushed onto the coding key context for the nested `Encoder`.
    /// - returns: The encoded representation of `value` or `nil` if it didn't encode anything.
    fileprivate func _encode<Value : Codable>(_ value: Value, pushingKey key: Key) throws -> Any? {
        if Value.self == Date.self {
            let date = value as! Date
            switch encoder.strategies.dateEncodingStrategy {
            case .secondsSince1970:
                return NSNumber(value: date.timeIntervalSince1970)

            case .millisecondsSince1970:
                return NSNumber(value: 1000.0 * date.timeIntervalSince1970)

            case .iso8601:
                if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
                    return _iso8601Formatter.string(from: date)
                } else {
                    fatalError("ISO8601DateFormatter is unavailable on this platform.")
                }

            case .formatted(let formatter):
                return formatter.string(from: date)

            case .custom(let closure):
                let innerEncoder = _JSONEncoder(strategies: encoder.strategies, context: codingKeyContext)
                try closure(date, innerEncoder)
                return innerEncoder.storage.value

            // This case falls through to the default logic below.
            case .deferredToDate: break
            }
        }

        var context = self.codingKeyContext
        context.append(key)

        let innerEncoder = _JSONEncoder(strategies: encoder.strategies, context: context)
        try value.encode(to: innerEncoder)
        return innerEncoder.storage.value
    }

    // MARK: - KeyedEncodingContainer Overrides

    override var codingKeyContext: [CodingKey] {
        return encoder.codingKeyContext
    }

    override func encode<Value : Codable>(_ value: Value?, forKey key: Key) throws {
        try encoder.storage.store(deferred: {
            if let value = value {
                return try _encode(value, pushingKey: key) ?? NSMutableDictionary()
            } else {
                return NSNull()
            }
        }, forKey: key)
    }

    override func encode(_ value: Bool?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Int?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Int8?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Int16?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Int32?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Int64?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: UInt?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: UInt8?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: UInt16?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: UInt32?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: UInt64?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : NSNumber(value: value!), forKey: key)
    }

    override func encode(_ value: Float?, forKey key: Key) throws {
        try encoder.storage.store(deferred: {
            guard let value = value else {
                return NSNull()
            }

            if let boxed = _box(value, strategy: encoder.strategies.nonConformingFloatEncodingStrategy) {
                return boxed
            } else {
                throw CocoaError.invalidFloatingPointValue(value, in: codingKeyContext)
            }
        }, forKey: key)
    }

    override func encode(_ value: Double?, forKey key: Key) throws {
        try encoder.storage.store(deferred: {
            guard let value = value else {
                return NSNull()
            }

            if let boxed = _box(value, strategy: encoder.strategies.nonConformingFloatEncodingStrategy) {
                return boxed
            } else {
                throw CocoaError.invalidFloatingPointValue(value, in: codingKeyContext)
            }
        }, forKey: key)
    }

    override func encode(_ value: String?, forKey key: Key) throws {
        encoder.storage.store(value == nil ? NSNull() : value!, forKey: key)
    }

    override func encode(_ value: Data?, forKey key: Key) throws {
        try encoder.storage.store(deferred: {
            guard let value = value else {
                return NSNull()
            }

            switch encoder.strategies.dataEncodingStrategy {
            case .base64Encode:
                return value.base64EncodedString()

            case .custom(let closure):
                let innerEncoder = _JSONEncoder(strategies: encoder.strategies, context: codingKeyContext)
                try closure(value, innerEncoder)
                return innerEncoder.storage.value ?? NSMutableDictionary()
            }
        }, forKey: key)
    }

    /// Returns a new `_JSONEncoder` referencing our encoder at the given key.
    ///
    /// - parameter key: The key the nested encoder should store values for.
    /// - returns: A new `_JSONEncoder` available for writing.
    func _nestedEncoder(forKey key: CodingKey) -> _JSONEncoder {
        var context = self.codingKeyContext
        context.append(key)
        return _JSONEncoder(storage: .reference(encoder, key), strategies: encoder.strategies, context: context)
    }

    override func nestedContainer<NestedKey : CodingKey>(keyedBy keyType: NestedKey.Type, type containerType: EncodingContainerType, forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        return _nestedEncoder(forKey: key).container(keyedBy: NestedKey.self, type: containerType)
    }

    override func superEncoder() -> Encoder {
        return _nestedEncoder(forKey: _JSONDefaultSuperKey())
    }

    override func superEncoder(forKey key: Key) -> Encoder {
        return _nestedEncoder(forKey: key)
    }
}

//===----------------------------------------------------------------------===//
// JSON Decoder
//===----------------------------------------------------------------------===//

/// `JSONDecoder` facilitates the decoding of JSON into semantic `Codable` types.
open class JSONDecoder {
    // MARK: - Options

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

    /// A container for strategies that can be passed around down the decoding hierarchy.
    fileprivate struct _Strategies {
        let dateDecodingStrategy: DateDecodingStrategy
        let dataDecodingStrategy: DataDecodingStrategy
        let nonConformingFloatDecodingStrategy: NonConformingFloatDecodingStrategy
    }

    /// The strategies set on the top-level decoder.
    fileprivate var strategies: _Strategies {
        return _Strategies(dateDecodingStrategy: dateDecodingStrategy,
                           dataDecodingStrategy: dataDecodingStrategy,
                           nonConformingFloatDecodingStrategy: nonConformingFloatDecodingStrategy)
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
    open func decode<Value : Codable>(_ type: Value.Type, from data: Data) throws -> Value {
        let topLevel = try JSONSerialization.jsonObject(with: data)
        let decoder = _JSONDecoder(storage: topLevel, strategies: self.strategies)
        return try Value(from: decoder)
    }
}

// MARK: -

fileprivate class _JSONDecoder : Decoder, SingleValueDecodingContainer {
    /// The storage to decode values from.
    let storage: Any

    /// The path of coding keys taken to get to this point in encoding.
    let codingKeyContext: [CodingKey]

    /// Strategies to use in converting values for encoding.
    let strategies: JSONDecoder._Strategies

    /// Initializes `self` with the given storage and strategies.
    ///
    /// - parameter storage: The value to reference for decoding.
    /// - parameter strategies: The strategies to use while decoding.
    /// - parameter context: The decoding context we're working in.
    init(storage: Any, strategies: JSONDecoder._Strategies, context: [CodingKey] = []) {
        self.storage = storage
        self.strategies = strategies
        self.codingKeyContext = context
    }

    // MARK: - Decoder Methods

    func container<Key : CodingKey>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> {
        guard storage is [Any] || storage is [String : Any] else {
            throw CocoaError.expectedContainer(in: codingKeyContext, reality: storage)
        }

        return _JSONKeyedDecodingContainer(referencing: self)
    }

    func singleValueContainer() throws -> SingleValueDecodingContainer {
        return self
    }

    // MARK: - SingleValueDecodingContainer Methods

    func decode(_ type: Bool.Type) throws -> Bool {
        return try _unbox(Bool.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Int.Type) throws -> Int {
        return try _unbox(Int.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Int8.Type) throws -> Int8 {
        return try _unbox(Int8.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Int16.Type) throws -> Int16 {
        return try _unbox(Int16.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Int32.Type) throws -> Int32 {
        return try _unbox(Int32.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Int64.Type) throws -> Int64 {
        return try _unbox(Int64.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: UInt.Type) throws -> UInt {
        return try _unbox(UInt.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: UInt8.Type) throws -> UInt8 {
        return try _unbox(UInt8.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: UInt16.Type) throws -> UInt16 {
        return try _unbox(UInt16.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: UInt32.Type) throws -> UInt32 {
        return try _unbox(UInt32.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: UInt64.Type) throws -> UInt64 {
        return try _unbox(UInt64.self, from: storage, in: codingKeyContext)
    }

    func decode(_ type: Float.Type) throws -> Float {
        return try _unbox(Float.self, from: storage, strategy: strategies.nonConformingFloatDecodingStrategy, in: codingKeyContext)
    }

    func decode(_ type: Double.Type) throws -> Double {
        return try _unbox(Double.self, from: storage, strategy: strategies.nonConformingFloatDecodingStrategy, in: codingKeyContext)
    }

    func decode(_ type: String.Type) throws -> String {
        guard let string = storage as? String else {
            throw CocoaError.typeMismatch(in: codingKeyContext, expectation: type, reality: storage)
        }

        return string
    }

    func decode(_ type: Data.Type) throws -> Data {
        switch strategies.dataDecodingStrategy {
        case .base64Decode:
            guard let string = storage as? String else {
                throw CocoaError.typeMismatch(in: codingKeyContext, expectation: type, reality: storage)
            }

            guard let data = Data(base64Encoded: string) else {
                throw CocoaError.coderReadCorrupt(in: codingKeyContext, reason: "Encountered Data string is not valid Base64.")
            }

            return data

        case .custom(let closure):
            return try closure(self)
        }
    }
}

fileprivate class _JSONKeyedDecodingContainer<Key : CodingKey> : KeyedDecodingContainer<Key> {
    /// A reference to the concrete decoder.
    let decoder: _JSONDecoder

    /// Initializes `self` by referencing the given decoder.
    ///
    /// - parameter decoder: The decoder to reference.
    init(referencing decoder: _JSONDecoder) {
        self.decoder = decoder
    }

    // MARK: - Storage Access

    /// Returns the value stored in the referenced decoder's storage for the given key.
    ///
    /// This method does not distinguish between the cases of "we don't have a value for this key" and "the value we found for this key is `nil`".
    /// That's because it's used to fetch values primarily for `decodeIfPresent(_:forKey:)`, which also doesn't make this distinction.
    ///
    /// - parameter key: The key to use when indexing into `decoder`.
    /// - returns: A value if present for the given key, or `nil` if the index was invalid or the value was null.
    fileprivate func _fetchValue(forKey key: CodingKey) -> Any? {
        let value: Any?
        if let array = decoder.storage as? [Any] {
            guard let index = key.intValue else {
                return nil
            }

            value = (0 ..< array.count).contains(index) ? array[index] : nil
        } else if let dictionary = decoder.storage as? [String : Any] {
            guard let name = _extractString(fromKey: key) else {
                return nil
            }

            value = dictionary[name]
        } else {
            // This should never happen -- we created a keyed decoding container out of a non-keyed value.
            fatalError("Cannot access non-keyed container by key.")
        }

        return value is NSNull ? nil : value
    }

    /// Returns a new Decoder wrapping the value at the given key in the referenced decoder's storage.
    ///
    /// - parameter key: The key to use when indexing into `decoder.storage`.
    /// - returns: A decoder wrapping the value at the given key, or `nil` if there is no value for that key, or the value is null.
    fileprivate func _nestedDecoder(forKey key: CodingKey) -> Decoder? {
        guard let value = _fetchValue(forKey: key) else {
            return nil
        }

        var context = decoder.codingKeyContext
        context.append(key)
        return _JSONDecoder(storage: value, strategies: decoder.strategies, context: context)
    }

    // MARK: - KeyedDecodingContainer Overrides

    override var codingKeyContext: [CodingKey] {
        return decoder.codingKeyContext
    }

    override var allKeys: [Key] {
        if let array = decoder.storage as? [Any] {
            // Attempt to map all array indices to `Key`; anything which doesn't map (producing `nil`) should be filtered out
            return (0 ..< array.count).flatMap { Key(intValue: $0) }
        } else if let dictionary = decoder.storage as? [String : Any] {
            // Attempt to map all keys to `Key`; anything which doesn't map (producing `nil`) should be filtered out
            return dictionary.keys.flatMap { (stringKey: String) in
                guard let mappedKey = Key(stringValue: stringKey) else {
                    // We weren't able to create a key out of this string. It's possible that this string is really an integer index that was stringified, so we need to attempt that as well.
                    let scanner = Scanner(string: stringKey)
                    var intKey: Int = -1
                    guard scanner.scanInt(&intKey) else {
                        return nil
                    }

                    return Key(intValue: intKey)
                }

                return mappedKey
            }
        } else {
            // This should never happen -- we created a keyed decoding container out of a non-keyed value.
            fatalError("Cannot access non-keyed container by key.")
        }
    }

    override func contains(_ key: Key) -> Bool {
        if let array = decoder.storage as? [Any] {
            guard let index = key.intValue else {
                // We don't have an entry for an invalid key, but this is not a fatal failure.
                return false
            }

            return (0 ..< array.count).contains(index)
        } else if let dictionary = decoder.storage as? [String : Any] {
            guard let name = _extractString(fromKey: key) else {
                // We don't have an entry for an invalid key, but this is not a fatal failure.
                return false
            }

            return dictionary[name] != nil
        } else {
            // This should never happen -- we created a keyed decoding container out of a non-keyed value.
            fatalError("Cannot access non-keyed container by key.")
        }
    }

    override func decodeIfPresent(_ type: Bool.Type, forKey key: Key) throws -> Bool? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Bool.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Int.Type, forKey key: Key) throws -> Int? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Int.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Int8.Type, forKey key: Key) throws -> Int8? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Int8.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Int16.Type, forKey key: Key) throws -> Int16? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Int16.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Int32.Type, forKey key: Key) throws -> Int32? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Int32.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Int64.Type, forKey key: Key) throws -> Int64? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Int64.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: UInt.Type, forKey key: Key) throws -> UInt? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(UInt.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: UInt8.Type, forKey key: Key) throws -> UInt8? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(UInt8.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: UInt16.Type, forKey key: Key) throws -> UInt16? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(UInt16.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: UInt32.Type, forKey key: Key) throws -> UInt32? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(UInt32.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: UInt64.Type, forKey key: Key) throws -> UInt64? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(UInt64.self, from: storage, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Float.Type, forKey key: Key) throws -> Float? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Float.self, from: storage, strategy: decoder.strategies.nonConformingFloatDecodingStrategy, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: Double.Type, forKey key: Key) throws -> Double? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        return try _unbox(Double.self, from: storage, strategy: decoder.strategies.nonConformingFloatDecodingStrategy, in: codingKeyContext)
    }

    override func decodeIfPresent(_ type: String.Type, forKey key: Key) throws -> String? {
        guard let storage = _fetchValue(forKey: key) else {
            return nil
        }

        guard let string = storage as? String else {
            throw CocoaError.typeMismatch(in: codingKeyContext, expectation: type, reality: storage)
        }

        return string
    }

    override func decodeIfPresent(_ type: Data.Type, forKey key: Key) throws -> Data? {
        switch decoder.strategies.dataDecodingStrategy {
        case .base64Decode:
            guard let storage = _fetchValue(forKey: key) else {
                return nil
            }

            guard let string = storage as? String else {
                throw CocoaError.typeMismatch(in: codingKeyContext, expectation: type, reality: storage)
            }

            guard let data = Data(base64Encoded: string) else {
                throw CocoaError.coderReadCorrupt(in: codingKeyContext, reason: "Encountered Data string is not valid Base64.")
            }

            return data
        case .custom(let closure):
            guard let decoderContainingData = _nestedDecoder(forKey: key) else {
                return nil
            }

            return try closure(decoderContainingData)
        }
    }

    override func decodeIfPresent<Value : Codable>(_ type: Value.Type, forKey key: Key) throws -> Value? {
        if Value.self == Date.self {
            // All (... as! Value) casts are needed here to silence the warning because Swift does not yet refine the type of Value into Date in cases like this.
            switch decoder.strategies.dateDecodingStrategy {
            case .secondsSince1970:
                guard let value = try decodeIfPresent(Double.self, forKey: key) else {
                    return nil
                }

                return (Date(timeIntervalSince1970: value) as! Value)

            case .millisecondsSince1970:
                guard let value = try decodeIfPresent(Double.self, forKey: key) else {
                    return nil
                }

                return (Date(timeIntervalSince1970: value / 1000.0) as! Value)

            case .iso8601:
                if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
                    guard let string = try decodeIfPresent(String.self, forKey: key) else {
                        return nil
                    }

                    guard let date = _iso8601Formatter.date(from: string) else {
                        throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Expected date string to be ISO8601-formatted.")
                    }

                    return (date as! Value)
                } else {
                    fatalError("ISO8601DateFormatter is unavailable on this platform.")
                }

            case .formatted(let formatter):
                guard let string = try decodeIfPresent(String.self, forKey: key) else {
                    return nil
                }

                guard let date = formatter.date(from: string) else {
                    throw CocoaError.coderReadCorrupt(in: decoder.codingKeyContext, reason: "Date string does not match format expected by formatter.")
                }

                return (date as! Value)

            case .custom(let closure):
                guard let nestedDecoder = _nestedDecoder(forKey: key) else {
                    return nil
                }

                return (try closure(nestedDecoder) as! Value)

            // This case falls through to the default logic below.
            case .deferredToDate:
                break
            }
        }

        guard let decoderContainingValue = _nestedDecoder(forKey: key) else {
            return nil
        }

        return try Value(from: decoderContainingValue)
    }

    override func nestedContainer<NestedKey : CodingKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
        guard let nestedDecoder = _nestedDecoder(forKey: key) else {
            throw CocoaError.coderValueNotFound(in: decoder.codingKeyContext, reason: "Unable to return nested container; there is no entry for the requested key.")
        }

        return try nestedDecoder.container(keyedBy: NestedKey.self)
    }

    override func superDecoder() throws -> Decoder {
        guard let superDecoder = _nestedDecoder(forKey: _JSONDefaultSuperKey()) else {
            throw CocoaError.coderValueNotFound(in: decoder.codingKeyContext, reason: "Unable to return super decoder; there is no entry for the requested key.")
        }

        return superDecoder
    }

    override func superDecoder(forKey key: Key) throws -> Decoder {
        guard let superDecoder = _nestedDecoder(forKey: key) else {
            throw CocoaError.coderValueNotFound(in: decoder.codingKeyContext, reason: "Unable to return super decoder; there is no entry for the requested key.")
        }

        return superDecoder
    }
}

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

// MARK: - Default Super Key

/// The default key to use in `superEncoder()` and `superDecoder()`.
/// Has a string value of "super" and an int value of 0.
fileprivate struct _JSONDefaultSuperKey : CodingKey {
    init() {}

    var stringValue: String? {
        return "super"
    }

    init?(stringValue: String) {
        guard stringValue == "super" else { return nil }
    }

    var intValue: Int? {
        return 0
    }

    init?(intValue: Int) {
        guard intValue == 0 else { return nil }
    }
}

// MARK: - Key Manipulation

/// Returns a the given key's string value, or stringifies its int value.
///
/// - parameter key: The key to extract.
/// - returns: A string representing the key if the key is not empty; `nil` otherwise.
fileprivate func _extractString(fromKey key: CodingKey) -> String? {
    if let stringValue = key.stringValue {
        return stringValue
    } else if let intValue = key.intValue {
        return "\(intValue)"
    } else {
        return nil
    }
}

// MARK: - Number Type Boxing.

/// Boxes the given `Float` value in either an `NSNumber` or a `String`, as appropriate for the value and strategy.
///
/// - parameter value: The value to box.
/// - parameter strategy: The encoding strategy to use if the value is not valid in JSON.
/// - returns: An `NSNumber` if the value is valid in JSON or a `String` if the strategy allows for the conversion of invalid numbers to strings; `nil` otherwise.
fileprivate func _box(_ value: Float, strategy: JSONEncoder.NonConformingFloatEncodingStrategy) -> Any? {
    if value.isInfinite || value.isNaN {
        guard case let .convertToString(positiveInfinity: posInfString, negativeInfinity: negInfString, nan: nanString) = strategy else {
            return nil
        }

        if value == Float.infinity {
            return posInfString
        } else if value == -Float.infinity {
            return negInfString
        } else {
            return nanString
        }
    } else {
        return NSNumber(value: value)
    }
}

/// Boxes the given `Double` value in either an `NSNumber` or a `String`, as appropriate for the value and strategy.
///
/// - parameter value: The value to box.
/// - parameter strategy: The encoding strategy to use if the value is not valid in JSON.
/// - returns: An `NSNumber` if the value is valid in JSON or a `String` if the strategy allows for the conversion of invalid numbers to strings; `nil` otherwise.
fileprivate func _box(_ value: Double, strategy: JSONEncoder.NonConformingFloatEncodingStrategy) -> Any? {
    if value.isInfinite || value.isNaN {
        guard case let .convertToString(positiveInfinity: posInfString, negativeInfinity: negInfString, nan: nanString) = strategy else {
            return nil
        }

        if value == Double.infinity {
            return posInfString
        } else if value == -Double.infinity {
            return negInfString
        } else {
            return nanString
        }
    } else {
        return NSNumber(value: value)
    }
}

// MARK: - Number Type Unboxing

/// Returns a boolean value out of the given value.
///
/// - parameter type: The type of value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A boolean value if the given value is `true`, `false`, `kCFBooleanTrue`, or `kCFBooleanFalse`.
/// - throws: `CocoaError.coderTypeMismatch` if the given value is not one of these values.
fileprivate func _unbox(_ type: Bool.Type, from value: Any, `in` context: [CodingKey]) throws -> Bool {
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

    throw CocoaError.typeMismatch(in: context, expectation: Bool.self, reality: value)
}

/// Returns an `Int` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Int.Type, from value: Any, `in` context: [CodingKey]) throws -> Int {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let int = number.intValue
    guard NSNumber(value: int) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return int
}

/// Returns an `Int8` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Int8.Type, from value: Any, `in` context: [CodingKey]) throws -> Int8 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let int8 = number.int8Value
    guard NSNumber(value: int8) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return int8
}

/// Returns an `Int16` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Int16.Type, from value: Any, `in` context: [CodingKey]) throws -> Int16 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let int16 = number.int16Value
    guard NSNumber(value: int16) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return int16
}

/// Returns an `Int32` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Int32.Type, from value: Any, `in` context: [CodingKey]) throws -> Int32 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let int32 = number.int32Value
    guard NSNumber(value: int32) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return int32
}

/// Returns an `Int64` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Int64.Type, from value: Any, `in` context: [CodingKey]) throws -> Int64 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let int64 = number.int64Value
    guard NSNumber(value: int64) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return int64
}

/// Returns a `UInt` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: UInt.Type, from value: Any, `in` context: [CodingKey]) throws -> UInt {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let uint = number.uintValue
    guard NSNumber(value: uint) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return uint
}

/// Returns a `UInt8` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: UInt8.Type, from value: Any, `in` context: [CodingKey]) throws -> UInt8 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let uint8 = number.uint8Value
    guard NSNumber(value: uint8) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return uint8
}

/// Returns a `UInt16` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: UInt16.Type, from value: Any, `in` context: [CodingKey]) throws -> UInt16 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let uint16 = number.uint16Value
    guard NSNumber(value: uint16) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return uint16
}

/// Returns a `UInt32` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: UInt32.Type, from value: Any, `in` context: [CodingKey]) throws -> UInt32 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let uint32 = number.uint32Value
    guard NSNumber(value: uint32) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return uint32
}

/// Returns a `UInt64` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber`.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: UInt64.Type, from value: Any, `in` context: [CodingKey]) throws -> UInt64 {
    // FIXME: If swift-corelibs-foundation is not updated to use NSNumber, check for Int and Double here.
    guard let number = value as? NSNumber else {
        throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
    }

    let uint64 = number.uint64Value
    guard NSNumber(value: uint64) == number else {
        throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
    }

    return uint64
}

/// Returns a `Float` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter strategy: The non-conforming floating-point decoding strategy to use.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber` or of the strategy allows for the decoding of strings and the string value does not match.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Float.Type, from value: Any, strategy: JSONDecoder.NonConformingFloatDecodingStrategy, `in` context: [CodingKey]) throws -> Float {
    if let number = value as? NSNumber {
        // We are willing to return a Float by losing precision:
        // * If the original value was integral,
        //   * and the integral value was > Float.greatestFiniteMagnitude, we will fail
        //   * and the integral value was <= Float.greatestFiniteMagnitude, we are willing to lose precision past 2^24
        // * If it was a Float, you will get back the precise value
        // * If it was a Double or Decimal, you will get back the nearest approximation if it will fit
        let double = number.doubleValue
        guard abs(double) <= Double(Float.greatestFiniteMagnitude) else {
            throw CocoaError.coderReadCorrupt(in: context, reason: "Parsed JSON number \(number) does not fit in \(type).")
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
        case .convertFromString(let posInfString, let negInfString, let nanString) = strategy {
        if string == posInfString {
            return Float.infinity
        } else if string == negInfString {
            return -Float.infinity
        } else if string == nanString {
            return Float.nan
        }
    }

    throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
}

/// Returns a `Double` out of the given value.
///
/// - parameter type: The type of the value to unbox.
/// - parameter value: The value to extract a value from.
/// - parameter strategy: The non-conforming floating-point decoding strategy to use.
/// - parameter context: The coding key context surrounding this value.
/// - returns: A decoded value of the requested type.
/// - throws: `CocoaError.coderTypeMismatch` if `value` is not an `NSNumber` or of the strategy allows for the decoding of strings and the string value does not match.
/// - throws: `CocoaError.coderReadCorrupt` if the integer value of `value` does not fit in the requested type.
fileprivate func _unbox(_ type: Double.Type, from value: Any, strategy: JSONDecoder.NonConformingFloatDecodingStrategy, `in` context: [CodingKey]) throws -> Double {
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
        case .convertFromString(let posInfString, let negInfString, let nanString) = strategy {
        if string == posInfString {
            return Double.infinity
        } else if string == negInfString {
            return -Double.infinity
        } else if string == nanString {
            return Double.nan
        }
    }

    throw CocoaError.typeMismatch(in: context, expectation: type, reality: value)
}

//===----------------------------------------------------------------------===//
// CocoaError Extensions
//===----------------------------------------------------------------------===//

extension CocoaError {
    /// Returns an error whose domain is `NSCocoaErrorDomain` and error is `CocoaError.coderInvalidValue`.
    ///
    /// - parameter value: The value that was invalid to encode.
    /// - parameter context: The context the value was attempted to be encoded in.
    /// - returns: An `Error` with the appropriate localized description, context, and debug description.
    fileprivate static func invalidFloatingPointValue<T : FloatingPoint>(_ value: T, `in` context: [CodingKey]) -> Error {
        let valueDescription: String
        if value == T.infinity {
            valueDescription = "\(T.self).infinity"
        } else if value == -T.infinity {
            valueDescription = "-\(T.self).infinity"
        } else {
            valueDescription = "\(T.self).nan"
        }

        let message = "Unable to encode \(valueDescription) directly in JSON. Use JSONEncoder.NonConformingFloatEncodingStrategy.convertToString to encode the value instead."
        return CocoaError.coderInvalidValue(in: context, reason: message)
    }

    /// Returns an error whose domain is `NSCocoaErrorDomain` and error is `Cocoa.coderTypeMismatch` describing a type mismatch.
    ///
    /// - parameter context: The context in which the error occurred.
    /// - parameter expectation: The type expected to be encountered.
    /// - parameter reality: The value that was encountered instead of the expected type.
    /// - returns: An error appropriate for throwing.
    fileprivate static func typeMismatch(`in` context: [CodingKey], expectation: Any.Type, reality: Any) -> Error {
        let message = "Expected to decode \(expectation) but found \(_typeDescription(of: reality)) instead."
        return CocoaError.coderTypeMismatch(in: context, reason: message)
    }

    /// Returns an error whose domain is `NSCocoaErrorDomain` and error is `Cocoa.coderTypeMismatch` describing a type mismatch.
    ///
    /// - parameter context: The context in which the error occurred.
    /// - parameter reality: The value that was encountered instead of the expected container.
    /// - returns: An error appropriate for throwing.
    fileprivate static func expectedContainer(`in` context: [CodingKey], reality: Any) -> Error {
        let message = "Expected to decode a container but found \(_typeDescription(of: reality)) instead."
        return CocoaError.coderTypeMismatch(in: context, reason: message)
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
            return "an array container"
        } else if value is [String : Any] {
            return "a dictionary container"
        } else {
            // This should never happen -- we somehow have a non-JSON type here.
            preconditionFailure("Invalid storage type \(type(of: value)).")
        }
    }
}
