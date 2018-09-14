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

public struct XPCSingleValueEncodingContainer: SingleValueEncodingContainer {
    // MARK: - Properties
    public var codingPath: [CodingKey] {
        get {
            return self.encoder.codingPath
        }
    }

    private let encoder: XPCEncoder
    private let insertionClosure: (_ value: xpc_object_t) throws -> ()

    // MARK: - Initialization
    init(referencing encoder: XPCEncoder, insertionClosure: @escaping (_ value: xpc_object_t) throws -> ()) {
        self.encoder = encoder
        self.insertionClosure = insertionClosure
    }

    // MARK: - SingleValueEncodingContainer protocol methods
    public mutating func encodeNil() throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeNil())
    }

    public mutating func encode(_ value: Bool) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeBool(value))
    }

    public mutating func encode(_ value: String) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeString(value))
    }

    public mutating func encode(_ value: Double) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeDouble(value))
    }

    public mutating func encode(_ value: Float) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeFloat(value))
    }

    public mutating func encode(_ value: Int) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int8) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int16) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int32) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: Int64) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeSignedInteger(value))
    }

    public mutating func encode(_ value: UInt) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt8) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt16) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt32) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode(_ value: UInt64) throws {
        try self.insertionClosure(XPCEncodingHelpers.encodeUnsignedInteger(value))
    }

    public mutating func encode<T>(_ value: T) throws where T : Encodable {
        let xpcObject = try XPCEncoder.encode(value, at: self.encoder.codingPath)
        try self.insertionClosure(xpcObject)
    }
}
