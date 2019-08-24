//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

extension NSRange : Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(location)
        hasher.combine(length)
    }

    public static func==(lhs: NSRange, rhs: NSRange) -> Bool {
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
    public init?(_ string: __shared String) {
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
        
        
        self.init(location: location, length: length)
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
  public init<R: RangeExpression>(_ region: R)
  where R.Bound: FixedWidthInteger {
    let r = region.relative(to: 0..<R.Bound.max)
    self.init(location: numericCast(r.lowerBound), length: numericCast(r.count))
  }
  
  public init<R: RangeExpression, S: StringProtocol>(_ region: R, in target: S)
  where R.Bound == S.Index {
    let r = region.relative(to: target)
    self.init(target._toUTF16Offsets(r))
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
  private init?<S: StringProtocol>(
    _ range: NSRange, _genericIn string: __shared S
  ) {
    // Corresponding stdlib version
    guard #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) else {
      fatalError()
    }
    let u = string.utf16
    guard range.location != NSNotFound,
      let start = u.index(
        u.startIndex, offsetBy: range.location, limitedBy: u.endIndex),
      let end = u.index(
        start, offsetBy: range.length, limitedBy: u.endIndex),
      let lowerBound = String.Index(start, within: string),
      let upperBound = String.Index(end, within: string)
    else { return nil }

    self = lowerBound..<upperBound
  }

  public init?(_ range: NSRange, in string: __shared String) {
    if #available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *) {
      // When building for an OS that has the new stuff that supports
      // the generic version available, we just use that.
      self.init(range, _genericIn: string)
    } else {
      // Need to have the old version available to support testing a just-
      // built standard library on 10.14. We may want to figure out a more
      // principled way to handle this in the future, but this should keep
      // local and PR testing working.
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
  
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public init?<S: StringProtocol>(_ range: NSRange, in string: __shared S) {
    self.init(range, _genericIn: string)
  }
}

extension NSRange : CustomReflectable {
    public var customMirror: Mirror {
        return Mirror(self, children: ["location": location, "length": length])
    }
}

extension NSRange : _CustomPlaygroundQuickLookable {
    @available(*, deprecated, message: "NSRange.customPlaygroundQuickLook will be removed in a future Swift version")
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
