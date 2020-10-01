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

@_exported import Foundation // Clang module
import Darwin.uuid
@_implementationOnly import _SwiftCoreFoundationOverlayShims

/// Represents UUID strings, which can be used to uniquely identify types, interfaces, and other items.
@available(macOS 10.8, iOS 6.0, *)
public struct UUID : ReferenceConvertible, Hashable, Equatable, CustomStringConvertible {
    public typealias ReferenceType = NSUUID

    public private(set) var uuid: uuid_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    /* Create a new UUID with RFC 4122 version 4 random bytes */
    public init() {
        withUnsafeMutablePointer(to: &uuid) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 16) {
                uuid_generate_random($0)
            }
        }
    }
    
    private init(reference: __shared NSUUID) {
        var bytes: uuid_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        withUnsafeMutablePointer(to: &bytes) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 16) {
                reference.getBytes($0)
            }
        }
        uuid = bytes
    }

    /// Create a UUID from a string such as "E621E1F8-C36C-495A-93FC-0C247A3E6E5F".
    /// 
    /// Returns nil for invalid strings.
    public init?(uuidString string: __shared String) {
        let res = withUnsafeMutablePointer(to: &uuid) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 16) {
                return uuid_parse(string, $0)
            }
        }
        if res != 0 {
            return nil
        }
    }
    
    /// Create a UUID from a `uuid_t`.
    public init(uuid: uuid_t) {
        self.uuid = uuid
    }
    
    /// Returns a string created from the UUID, such as "E621E1F8-C36C-495A-93FC-0C247A3E6E5F"
    public var uuidString: String {
        var bytes: uuid_string_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        return withUnsafePointer(to: uuid) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 16) { val in
                withUnsafeMutablePointer(to: &bytes) {
                    $0.withMemoryRebound(to: Int8.self, capacity: 37) { str in
                        uuid_unparse(val, str)
                        return String(cString: UnsafePointer(str), encoding: .utf8)!
                    }
                }
            }
        }
    }
    
    public func hash(into hasher: inout Hasher) {
        withUnsafeBytes(of: uuid) { buffer in
            hasher.combine(bytes: buffer)
        }
    }

    public var description: String {
        return uuidString
    }

    public var debugDescription: String {
        return description
    }
    
    // MARK: - Bridging Support
    
    private var reference: NSUUID {
        return withUnsafePointer(to: uuid) {
            $0.withMemoryRebound(to: UInt8.self, capacity: 16) {
                return NSUUID(uuidBytes: $0)
            }
        }
    }

    public static func ==(lhs: UUID, rhs: UUID) -> Bool {
        return withUnsafeBytes(of: rhs.uuid) { (rhsPtr) -> Bool in
            return withUnsafeBytes(of: lhs.uuid) { (lhsPtr) -> Bool in
                let lhsFirstChunk = lhsPtr.load(fromByteOffset: 0, as: UInt64.self)
                let lhsSecondChunk = lhsPtr.load(fromByteOffset: MemoryLayout<UInt64>.size, as: UInt64.self)
                let rhsFirstChunk = rhsPtr.load(fromByteOffset: 0, as: UInt64.self)
                let rhsSecondChunk = rhsPtr.load(fromByteOffset: MemoryLayout<UInt64>.size, as: UInt64.self)
                return ((lhsFirstChunk ^ rhsFirstChunk) | (lhsSecondChunk ^ rhsSecondChunk)) == 0
            }
        }
    }
}

extension UUID : CustomReflectable {
    public var customMirror: Mirror {
        let c : [(label: String?, value: Any)] = []
        let m = Mirror(self, children:c, displayStyle: Mirror.DisplayStyle.struct)
        return m
    }
}

extension UUID : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSUUID {
        return reference
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSUUID, result: inout UUID?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSUUID, result: inout UUID?) -> Bool {
        result = UUID(reference: input)
        return true
    }

    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSUUID?) -> UUID {
        var result: UUID?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSUUID : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as UUID)
    }
}

extension UUID : Codable {
    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let uuidString = try container.decode(String.self)

        guard let uuid = UUID(uuidString: uuidString) else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Attempted to decode UUID from invalid UUID string."))
        }

        self = uuid
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.uuidString)
    }
}
