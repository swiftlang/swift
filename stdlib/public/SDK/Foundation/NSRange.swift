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

extension NSRange : Hashable {
    public var hashValue: Int {
#if arch(i386) || arch(arm)
        return Int(bitPattern: (UInt(bitPattern: location) | (UInt(bitPattern: length) << 16)))
#elseif arch(x86_64) || arch(arm64)
        return Int(bitPattern: (UInt(bitPattern: location) | (UInt(bitPattern: length) << 32)))
#endif
    }

    public static func==(_ lhs: NSRange, _ rhs: NSRange) -> Bool {
        return lhs.location == rhs.location && rhs.length == rhs.length
    }
}

extension NSRange : CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String { return "{\(location), \(length)}" }
    public var debugDescription: String { return "{\(location), \(length)}" }
}

extension NSRange {
    public init?(_ string: String) {
        if string.isEmpty {
            // fail early if the string is empty
            return nil
        }
        let scanner = Scanner(string: string)
        let digitSet = CharacterSet.decimalDigits
        let _ = scanner.scanUpToCharacters(from: digitSet, into: nil)
        if scanner.isAtEnd {
            // fail early if there are no decimal digits
            return nil
        }
        var location = 0
        guard scanner.scanInt(&location) else {
            return nil
        }
        if scanner.isAtEnd {
            // return early if there are no more characters after the first int in the string
            return nil
        }
        let _ = scanner.scanUpToCharacters(from: digitSet, into: nil)
        if scanner.isAtEnd {
            // return early if there are no integer characters after the first int in the string
            return nil
        }
        var length = 0
        guard scanner.scanInt(&length) else {
            return nil
        }
        
        self.location = location
        self.length = length
    }
}

extension NSRange {
    public var lowerBound: Int { return location }
    
    public var upperBound: Int { return location + length }
    
    public func contains(_ index: Int) -> Bool { return (!(index < location) && (index - location) < length) }
    
    public mutating func formUnion(_ other: NSRange) {
        self = union(other)
    }

    public func union(_ other: NSRange) -> NSRange {
        let max1 = location + length
        let max2 = other.location + other.length
        let maxend = (max1 < max2) ? max2 : max1
        let minloc = location < other.location ? location : other.location
        return NSRange(location: minloc, length: maxend - minloc)
    }

    public func intersection(_ other: NSRange) -> NSRange? {
        let max1 = location + length
        let max2 = other.location + other.length
        let minend = (max1 < max2) ? max1 : max2
        if other.location <= location && location < max2 {
            return NSRange(location: location, length: minend - location)
        } else if location <= other.location && other.location < max1 {
            return NSRange(location: other.location, length: minend - other.location);
        }
        return nil
    }
}


//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension NSRange {
    public init(_ x: Range<Int>) {
        location = x.lowerBound
        length = x.count
    }

    // FIXME(ABI)#75 (Conditional Conformance): this API should be an extension on Range.
    // Can't express it now because the compiler does not support conditional
    // extensions with type equality constraints.
    public func toRange() -> Range<Int>? {
        if location == NSNotFound { return nil }
        return location..<(location+length)
    }
}

extension NSRange : CustomReflectable {
    public var customMirror: Mirror {
        return Mirror(self, children: ["location": location, "length": length])
    }
}

extension NSRange : CustomPlaygroundQuickLookable {
    public var customPlaygroundQuickLook: PlaygroundQuickLook {
        return .range(Int64(location), Int64(length))
    }
}

extension NSRange : Codable {
    public init(from decoder: Decoder) throws {
        var container = try decoder.unkeyedContainer()
        let location = try container.decode(Int.self)
        let length = try container.decode(Int.self)
        self.init(location: location, length: length)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.unkeyedContainer()
        try container.encode(self.location)
        try container.encode(self.length)
    }
}
