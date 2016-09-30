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

extension _NSContiguousString {
    @objc
    var length: Int {
        return _core.count
    }

    @objc(characterAtIndex:)
    func character(at index: Int) -> unichar {
        return _core[index]
    }

    @inline(__always) // Performance: To save on reference count operations.
    @objc(getCharacters:range:)
    func getCharacters(_ buffer: UnsafeMutablePointer<unichar>, range: NSRange) {

        _precondition(range.location + range.length <= Int(_core.count))

        if _core.elementWidth == 2 {
            UTF16._copy(
                source: _core.startUTF16 + range.location,
                destination: UnsafeMutablePointer<UInt16>(buffer),
                count: range.length)
        }
        else {
            UTF16._copy(
                source: _core.startASCII + range.location,
                destination: UnsafeMutablePointer<UInt16>(buffer),
                count: range.length)
        }
    }

    //
    // Implement sub-slicing without adding layers of wrapping
    //
    @objc(substringFromIndex:)
    func substring(from start: Int) -> _NSContiguousString {
        return _NSContiguousString(_core[Int(start)..<Int(_core.count)])
    }

    @objc(substringToIndex:)
    func substring(to end: Int) -> _NSContiguousString {
        return _NSContiguousString(_core[0..<Int(end)])
    }

    @objc(substringWithRange:)
    func substring(with range: NSRange) -> _NSContiguousString {
        return _NSContiguousString(
            _core[Int(range.location)..<Int(range.location + range.length)])
    }

    @objc(copyWithZone:)
    func copy(with zone: NSZone? = nil) -> Any {
        // Since this string is immutable we can just return ourselves.
        return self
    }

    @objc(copy)
    func copy() -> Any {
        return self
    }

    @objc
    var fastestEncoding: UInt {
        return _core.elementWidth == 1 ? String.Encoding.ascii.rawValue : String.Encoding.utf16.rawValue
    }

    @objc
    var smallestEncoding: UInt {
        return _core.elementWidth == 1 ? String.Encoding.ascii.rawValue : String.Encoding.utf16.rawValue
    }

    /// The following methods are purely optimizations as special dispensation from
    /// Foundation. Relying on these methods to exist, return sane results and/or being
    /// used in production code is strictly not supported. They may change behavior in
    /// future releases and are purely SPI for authorized consumers.
    @objc(_fastCharacterContents)
    func _fastCharacterContents() -> UnsafePointer<unichar>? {
        return _core.elementWidth == 2 ? UnsafePointer<unichar>(_core.startUTF16) : nil
    }

    @objc(_fastCStringContents:)
    func _fastCStringContents(_ nullTerminationRequired: Bool) -> UnsafePointer<Int8>? {
        // Note: this memory should NOT be rebound since it is immediately being returned to ObjC scope
        return _core.elementWidth == 1 ? unsafeBitCast(_core.startASCII, to: UnsafePointer<Int8>.self) : nil
    }
}
