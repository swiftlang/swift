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
    @objc(copyWithZone:)
    func copy(with zone: NSZone? = nil) -> Any {
        // Since this string is immutable we can just return ourselves.
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
    func _fastCharacterContents() -> UnsafeRawPointer? {
        return _core.elementWidth == 2 ? UnsafeRawPointer(_core.startUTF16) : nil
    }
    
    @objc(_fastCStringContents:)
    func _fastCStringContents(_ nullTerminationRequired: Bool) -> UnsafeRawPointer? {
        guard _core.isASCII else {
            return nil
        }
        guard nullTerminationRequired else {
            return UnsafeRawPointer(_core.startASCII)
        }
        let cnt = _core.count
        guard cnt != 0 else { 
            return nil
        }
        // This depends on the underlying buffer either being a slice or a null terminated buffer 
        // because it will read off the end of the slice by one byte...
        guard _core.startASCII[cnt] == 0 else {
            return nil
        }
        return UnsafeRawPointer(_core.startASCII)
    }
}
