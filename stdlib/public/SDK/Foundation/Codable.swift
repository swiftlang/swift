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
// Codable Extensions
//===----------------------------------------------------------------------===//

extension Date : Codable {
    public init(from decoder: Decoder) throws {
        let timestamp = try decoder.singleValueContainer().decode(Double.self)
        self.init(timeIntervalSinceReferenceDate: timestamp)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.timeIntervalSinceReferenceDate)
    }
}

extension Data : Codable {
    private enum CodingKeys : Int, CodingKey {
        case length
        case bytes
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let length = try container.decode(Int.self, forKey: .length)
        guard length >= 0 else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Cannot decode a Data of negative length \(length)."))
        }

        var bytesContainer = try container.nestedUnkeyedContainer(forKey: .bytes)

        self.init(capacity: length)
        for i in 0 ..< length {
            let byte = try bytesContainer.decode(UInt8.self)
            self[i] = byte
        }
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(self.count, forKey: .length)

        var bytesContainer = container.nestedUnkeyedContainer(forKey: .bytes)

        // Since enumerateBytes does not rethrow, we need to catch the error, stow it away, and rethrow if we stopped.
        var caughtError: Error? = nil
        self.enumerateBytes { (buffer: UnsafeBufferPointer<UInt8>, byteIndex: Data.Index, stop: inout Bool) in
            do {
                try bytesContainer.encode(contentsOf: buffer)
            } catch {
                caughtError = error
                stop = true
            }
        }

        if let error = caughtError {
            throw error
        }
    }
}

//===----------------------------------------------------------------------===//
// Errors
//===----------------------------------------------------------------------===//

// Adding the following extensions to EncodingError and DecodingError allows them to bridge to NSErrors implicitly.

fileprivate let NSCodingPathErrorKey = "NSCodingPath"
fileprivate let NSDebugDescriptionErrorKey = "NSDebugDescription"

extension EncodingError : CustomNSError {
    public static var errorDomain: String = NSCocoaErrorDomain

    public var errorCode: Int {
        switch self {
        case .invalidValue(_, _): return CocoaError.coderInvalidValue.rawValue
        }
    }

    public var errorUserInfo: [String : Any] {
        let context: Context
        switch self {
        case .invalidValue(_, let c): context = c
        }

        return [NSCodingPathErrorKey: context.codingPath,
                NSDebugDescriptionErrorKey: context.debugDescription]
    }
}

extension DecodingError : CustomNSError {
    public static var errorDomain: String = NSCocoaErrorDomain

    public var errorCode: Int {
        switch self {
        case .valueNotFound(_, _): fallthrough
        case .keyNotFound(_, _):
            return CocoaError._coderValueNotFound.rawValue

        case .typeMismatch(_, _): fallthrough
        case .dataCorrupted(_):
            return CocoaError._coderReadCorrupt.rawValue
        }
    }

    public var errorUserInfo: [String : Any]? {
        let context: Context
        switch self {
        case .typeMismatch(_, let c): context = c
        case .valueNotFound(_, let c): context = c
        case .keyNotFound(_, let c): context = c
        case .dataCorrupted(let c): context = c
        }

        return [NSCodingPathErrorKey: context.codingPath,
                NSDebugDescriptionErrorKey: context.debugDescription]
    }
}

//===----------------------------------------------------------------------===//
// Error Utilities
//===----------------------------------------------------------------------===//

internal extension DecodingError {
    /// Returns a `.typeMismatch` error describing the expected type.
    ///
    /// - parameter path: The path of `CodingKey`s taken to decode a value of this type.
    /// - parameter expectation: The type expected to be encountered.
    /// - parameter reality: The value that was encountered instead of the expected type.
    /// - returns: A `DecodingError` with the appropriate path and debug description.
    internal static func _typeMismatch(at path: [CodingKey?], expectation: Any.Type, reality: Any) -> DecodingError {
        let description = "Expected to decode \(expectation) but found \(_typeDescription(of: reality)) instead."
        return .typeMismatch(expectation, Context(codingPath: path, debugDescription: description))
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
