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
        return lhs.location == rhs.location && lhs.length == rhs.length
    }
}

extension NSRange : CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String { return "{\(location), \(length)}" }
    public var debugDescription: String {
        guard location != NSNotFound else {
            return "{NSNotFound, \(length)}"     
        }
        return "{\(location), \(length)}" 
    }
}

extension NSRange {
    public init?(_ string: String) {
        var savedLocation = 0
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
        savedLocation = scanner.scanLocation
        guard scanner.scanInt(&location) else {
            return nil
        }
        if scanner.isAtEnd {
            // return early if there are no more characters after the first int in the string
            return nil
        }
        if scanner.scanString(".", into: nil) {
            scanner.scanLocation = savedLocation
            var double = 0.0
            guard scanner.scanDouble(&double) else {
                return nil
            }
            guard let integral = Int(exactly: double) else {
                return nil
            }
            location = integral
        }
        
        let _ = scanner.scanUpToCharacters(from: digitSet, into: nil)
        if scanner.isAtEnd {
            // return early if there are no integer characters after the first int in the string
            return nil
        }
        var length = 0
        savedLocation = scanner.scanLocation
        guard scanner.scanInt(&length) else {
            return nil
        }
        
        if !scanner.isAtEnd {
            if scanner.scanString(".", into: nil) {
                scanner.scanLocation = savedLocation
                var double = 0.0
                guard scanner.scanDouble(&double) else {
                    return nil
                }
                guard let integral = Int(exactly: double) else {
                    return nil
                }
                length = integral
            }
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
  public init<R: RangeExpression>(_ rangeExpression: R)
  where R.Bound: FixedWidthInteger, R.Bound.Stride : SignedInteger {
    let range = rangeExpression.relative(to: 0..<R.Bound.max)
    let start: Int = numericCast(range.lowerBound)
    let end: Int = numericCast(range.upperBound)
    self = NSRange(location: start, length: end - start)
  }
  
  public init<R: RangeExpression, S: StringProtocol>(_ rangeExpression: R, in string: S)
  where R.Bound == String.Index, S.Index == String.Index {
    let range = rangeExpression.relative(to: string)
    let start = range.lowerBound.samePosition(in: string.utf16)
    let end = range.upperBound.samePosition(in: string.utf16)
    let location = string.utf16.distance(from: string.utf16.startIndex, to: start)
    let length = string.utf16.distance(from: start, to: end)
    self = NSRange(location: location, length: length)
  }

  @available(swift, deprecated: 4, renamed: "Range.init(_:)")
  public func toRange() -> Range<Int>? {
      if location == NSNotFound { return nil }
      return location..<(location+length)
  }
}

extension Range where Bound: BinaryInteger {
  public init?(_ range: NSRange) {
    guard range.location != NSNotFound else { return nil }
    self.init(uncheckedBounds: (numericCast(range.lowerBound), numericCast(range.upperBound)))
  }
}

// This additional overload will mean Range.init(_:) defaults to Range<Int> when
// no additional type context is provided:
extension Range where Bound == Int {
  public init?(_ range: NSRange) {
    guard range.location != NSNotFound else { return nil }
    self.init(uncheckedBounds: (range.lowerBound, range.upperBound))
  }
}

extension Range where Bound == String.Index {
  public init?(_ range: NSRange, in string: String) {
    let u = string.utf16
    guard range.location != NSNotFound,
      let start = u.index(u.startIndex, offsetBy: range.location, limitedBy: u.endIndex),
      let end = u.index(u.startIndex, offsetBy: range.location + range.length, limitedBy: u.endIndex),
      let lowerBound = String.Index(start, within: string),
      let upperBound = String.Index(end, within: string)
    else { return nil }
    
    self = lowerBound..<upperBound
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
