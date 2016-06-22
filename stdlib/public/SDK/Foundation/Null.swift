//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

/// Used to represent null values in collection objects (which don’t allow `nil` values).
@available(OSX 10.8, iOS 6.0, *)
public struct Null : ReferenceConvertible, Hashable, Equatable, CustomStringConvertible {
    
    public typealias ReferenceType = NSNull
    
    /// Private singleton for better performance.
    private static let _nsNull = NSNull()
    
    // MARK: - Intialization
    
    public init() { }
    
    // MARK: - Properties
    
    public var hashValue: Int {
        
        return Null._nsNull.hashValue
    }
    
    public var description: String {
        
        return Null._nsNull.description
    }

    public var debugDescription: String {
        
        return Null._nsNull.debugDescription
    }
    
    // MARK: - Bridging Support
    
    private var reference: NSNull {
        
        return Null._nsNull
    }
}

// MARK: - Equatable

public func ==(lhs: Null, rhs: Null) -> Bool {
    
    return true
}

// MARK: - _ObjectiveCBridgeable

extension Null : _ObjectiveCBridgeable {
    public static func _isBridgedToObjectiveC() -> Bool {
        return true
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNull {
        return reference
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNull, result: inout Null?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSNull, result: inout Null?) -> Bool {
        result = Null()
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNull?) -> Null {
        var result: Null? = nil
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}
