// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

extension CharacterSet {
    fileprivate func contains(_ character: Character) -> Bool {
        return character.unicodeScalars.allSatisfy(self.contains(_:))
    }
}

// -----

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension Scanner {
    public enum NumberRepresentation {
        case decimal // See the %d, %f and %F format conversions.
        case hexadecimal // See the %x, %X, %a and %A format conversions. For integers, a leading 0x or 0X is optional; for floating-point numbers, it is required.
    }
    
    public var currentIndex: String.Index {
        get {
            let string = self.string
            var index = string._toUTF16Index(scanLocation)
            
            var delta = 0
            while index != string.endIndex && index.samePosition(in: string) == nil {
                delta += 1
                index = string._toUTF16Index(scanLocation + delta)
            }
            
            return index
        }
        set { scanLocation = string._toUTF16Offset(newValue) }
    }
    
    fileprivate func _scan<Integer: FixedWidthInteger>
        (representation: NumberRepresentation,
         scanDecimal: (UnsafeMutablePointer<Integer>?) -> Bool,
         scanHexadecimal: (UnsafeMutablePointer<Integer>?) -> Bool) -> Integer? {
        var value: Integer = .max
        
        switch representation {
        case .decimal: guard scanDecimal(&value) else { return nil }
        case .hexadecimal: guard scanHexadecimal(&value) else { return nil }
        }
        
        return value
    }
    
    fileprivate func _scan<FloatingPoint: BinaryFloatingPoint>
        (representation: NumberRepresentation,
         scanDecimal: (UnsafeMutablePointer<FloatingPoint>?) -> Bool,
         scanHexadecimal: (UnsafeMutablePointer<FloatingPoint>?) -> Bool) -> FloatingPoint? {
        var value: FloatingPoint = .greatestFiniteMagnitude
        
        switch representation {
        case .decimal: guard scanDecimal(&value) else { return nil }
        case .hexadecimal: guard scanHexadecimal(&value) else { return nil }
        }
        
        return value
    }
    
    fileprivate func _scan<Integer: FixedWidthInteger, OverflowingHexadecimalInteger: FixedWidthInteger & UnsignedInteger>
        (representation: NumberRepresentation,
         scanDecimal: (UnsafeMutablePointer<Integer>?) -> Bool,
         overflowingScanHexadecimal: (UnsafeMutablePointer<OverflowingHexadecimalInteger>?) -> Bool) -> Integer? {
        return _scan(representation: representation, scanDecimal: scanDecimal, scanHexadecimal: { (pointer) -> Bool in
            var unsignedValue: OverflowingHexadecimalInteger = .max
            guard overflowingScanHexadecimal(&unsignedValue) else { return false }
            if unsignedValue <= Integer.max {
                pointer?.pointee = Integer(unsignedValue)
            }
            return true
        })
    }
    
    public func scanInt(representation: NumberRepresentation = .decimal) -> Int? {
#if arch(x86_64) || arch(arm64) || arch(s390x) || arch(powerpc64) || arch(powerpc64le)
        if let value = scanInt64(representation: representation) { 
            return Int(value)
        }
#elseif arch(i386) || arch(arm)
        if let value = scanInt32(representation: representation) { 
            return Int(value)
        }
#else
    #error("This architecture isn't known. Add it to the 32-bit or 64-bit line; if the machine word isn't either of those, you need to implement appropriate scanning and handle the potential overflow here.")
#endif
        return nil
    }
    
    public func scanInt32(representation: NumberRepresentation = .decimal) -> Int32? {
        return _scan(representation: representation, scanDecimal: self.scanInt32(_:), overflowingScanHexadecimal: self.scanHexInt32(_:))
    }
    
    public func scanInt64(representation: NumberRepresentation = .decimal) -> Int64? {
        return _scan(representation: representation, scanDecimal: self.scanInt64(_:), overflowingScanHexadecimal: self.scanHexInt64(_:))
    }
    
    public func scanUInt64(representation: NumberRepresentation = .decimal) -> UInt64? {
        return _scan(representation: representation, scanDecimal: self.scanUnsignedLongLong(_:), scanHexadecimal: self.scanHexInt64(_:))
    }
    
    public func scanFloat(representation: NumberRepresentation = .decimal) -> Float? {
        return _scan(representation: representation, scanDecimal: self.scanFloat(_:), scanHexadecimal: self.scanHexFloat(_:))
    }
    
    public func scanDouble(representation: NumberRepresentation = .decimal) -> Double? {
        return _scan(representation: representation, scanDecimal: self.scanDouble(_:), scanHexadecimal: self.scanHexDouble(_:))
    }
    
    public func scanDecimal() -> Decimal? {
        var value: Decimal = 0
        guard scanDecimal(&value) else { return nil }
        return value
    }
    
    
    fileprivate var _currentIndexAfterSkipping: String.Index {
        guard let skips = charactersToBeSkipped else { return currentIndex }
        
        let index = string[currentIndex...].firstIndex(where: { !skips.contains($0) })
        return index ?? string.endIndex
    }
    
    public func scanString(_ searchString: String) -> String? {
        let currentIndex = _currentIndexAfterSkipping
        
        guard let substringEnd = string.index(currentIndex, offsetBy: searchString.count, limitedBy: string.endIndex) else { return nil }
        
        if string.compare(searchString, options: self.caseSensitive ? [] : .caseInsensitive, range: currentIndex ..< substringEnd, locale: self.locale as? Locale) == .orderedSame {
            let it = string[currentIndex ..< substringEnd]
            self.currentIndex = substringEnd
            return String(it)
        } else {
            return nil
        }
    }
    
    public func scanCharacters(from set: CharacterSet) -> String? {
        let currentIndex = _currentIndexAfterSkipping
        
        let substringEnd = string[currentIndex...].firstIndex(where: { !set.contains($0) }) ?? string.endIndex
        guard currentIndex != substringEnd else { return nil }
        
        let substring = string[currentIndex ..< substringEnd]
        self.currentIndex = substringEnd
        return String(substring)
    }
    
    public func scanUpToString(_ substring: String) -> String? {
        guard !substring.isEmpty else { return nil }
        let string = self.string
        let startIndex = _currentIndexAfterSkipping

        var beginningOfNewString = string.endIndex
        var currentSearchIndex = startIndex
        
        repeat {
            guard let range = string.range(of: substring, options: self.caseSensitive ? [] : .caseInsensitive, range: currentSearchIndex ..< string.endIndex, locale: self.locale as? Locale) else {
                // If the string isn't found at all, it means it's not in the string. Just take everything to the end.
                beginningOfNewString = string.endIndex
                break
            }
            
            // range(of:…) can return partial grapheme ranges when dealing with emoji.
            // Make sure we take a range only if it doesn't split a grapheme in the string.
            if let maybeBeginning = range.lowerBound.samePosition(in: string),
                range.upperBound.samePosition(in: string) != nil {
                beginningOfNewString = maybeBeginning
                break
            }
            
            // If we got here, we need to search again starting from just after the location we found.
            currentSearchIndex = range.upperBound
        } while beginningOfNewString == string.endIndex && currentSearchIndex < string.endIndex
        
        guard startIndex != beginningOfNewString else { return nil }
        
        let foundSubstring = string[startIndex ..< beginningOfNewString]
        self.currentIndex = beginningOfNewString
        return String(foundSubstring)
    }
    
    public func scanUpToCharacters(from set: CharacterSet) -> String? {
        let currentIndex = _currentIndexAfterSkipping
        let string = self.string
        
        let firstCharacterInSet = string[currentIndex...].firstIndex(where: { set.contains($0) }) ?? string.endIndex
        guard currentIndex != firstCharacterInSet else { return nil }
        self.currentIndex = firstCharacterInSet
        return String(string[currentIndex ..< firstCharacterInSet])
    }
    
    public func scanCharacter() -> Character? {
        let currentIndex = _currentIndexAfterSkipping
        
        let string = self.string
        
        guard currentIndex != string.endIndex else { return nil }
        
        let character = string[currentIndex]
        self.currentIndex = string.index(after: currentIndex)
        return character
    }
}
