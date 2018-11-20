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

// FIXME: one day this will be bridged from CoreFoundation and we
// should drop it here. <rdar://problem/14497260> (need support
// for CF bridging)
public var kCFStringEncodingASCII: CFStringEncoding { return 0x0600 }

extension String {
  public struct Encoding : RawRepresentable {
    public var rawValue: UInt
    public init(rawValue: UInt) { self.rawValue = rawValue }

    public static let ascii = Encoding(rawValue: 1)
    public static let nextstep = Encoding(rawValue: 2)
    public static let japaneseEUC = Encoding(rawValue: 3)
    public static let utf8 = Encoding(rawValue: 4)
    public static let isoLatin1 = Encoding(rawValue: 5)
    public static let symbol = Encoding(rawValue: 6)
    public static let nonLossyASCII = Encoding(rawValue: 7)
    public static let shiftJIS = Encoding(rawValue: 8)
    public static let isoLatin2 = Encoding(rawValue: 9)
    public static let unicode = Encoding(rawValue: 10)
    public static let windowsCP1251 = Encoding(rawValue: 11)
    public static let windowsCP1252 = Encoding(rawValue: 12)
    public static let windowsCP1253 = Encoding(rawValue: 13)
    public static let windowsCP1254 = Encoding(rawValue: 14)
    public static let windowsCP1250 = Encoding(rawValue: 15)
    public static let iso2022JP = Encoding(rawValue: 21)
    public static let macOSRoman = Encoding(rawValue: 30)
    public static let utf16 = Encoding.unicode
    public static let utf16BigEndian = Encoding(rawValue: 0x90000100)
    public static let utf16LittleEndian = Encoding(rawValue: 0x94000100)
    public static let utf32 = Encoding(rawValue: 0x8c000100)
    public static let utf32BigEndian = Encoding(rawValue: 0x98000100)
    public static let utf32LittleEndian = Encoding(rawValue: 0x9c000100)
  }

  public typealias EncodingConversionOptions = NSString.EncodingConversionOptions
  public typealias EnumerationOptions = NSString.EnumerationOptions
  public typealias CompareOptions = NSString.CompareOptions
}

extension String.Encoding : Hashable {
    public var hashValue : Int { // FIXME(hashValue): Remove
        return rawValue.hashValue
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(rawValue)
    }

    public static func ==(lhs: String.Encoding, rhs: String.Encoding) -> Bool {
        return lhs.rawValue == rhs.rawValue
    }
}

extension String.Encoding : CustomStringConvertible {
  public var description: String {
    return String.localizedName(of: self)
  }
}

@available(*, unavailable, renamed: "String.Encoding")
public typealias NSStringEncoding = UInt

@available(*, unavailable, renamed: "String.Encoding.ascii")
public var NSASCIIStringEncoding: String.Encoding {
  return String.Encoding.ascii
}
@available(*, unavailable, renamed: "String.Encoding.nextstep")
public var NSNEXTSTEPStringEncoding: String.Encoding {
  return String.Encoding.nextstep
}
@available(*, unavailable, renamed: "String.Encoding.japaneseEUC")
public var NSJapaneseEUCStringEncoding: String.Encoding {
  return String.Encoding.japaneseEUC
}
@available(*, unavailable, renamed: "String.Encoding.utf8")
public var NSUTF8StringEncoding: String.Encoding {
  return String.Encoding.utf8
}
@available(*, unavailable, renamed: "String.Encoding.isoLatin1")
public var NSISOLatin1StringEncoding: String.Encoding {
  return String.Encoding.isoLatin1
}
@available(*, unavailable, renamed: "String.Encoding.symbol")
public var NSSymbolStringEncoding: String.Encoding {
  return String.Encoding.symbol
}
@available(*, unavailable, renamed: "String.Encoding.nonLossyASCII")
public var NSNonLossyASCIIStringEncoding: String.Encoding {
  return String.Encoding.nonLossyASCII
}
@available(*, unavailable, renamed: "String.Encoding.shiftJIS")
public var NSShiftJISStringEncoding: String.Encoding {
  return String.Encoding.shiftJIS
}
@available(*, unavailable, renamed: "String.Encoding.isoLatin2")
public var NSISOLatin2StringEncoding: String.Encoding {
  return String.Encoding.isoLatin2
}
@available(*, unavailable, renamed: "String.Encoding.unicode")
public var NSUnicodeStringEncoding: String.Encoding {
  return String.Encoding.unicode
}
@available(*, unavailable, renamed: "String.Encoding.windowsCP1251")
public var NSWindowsCP1251StringEncoding: String.Encoding {
  return String.Encoding.windowsCP1251
}
@available(*, unavailable, renamed: "String.Encoding.windowsCP1252")
public var NSWindowsCP1252StringEncoding: String.Encoding {
  return String.Encoding.windowsCP1252
}
@available(*, unavailable, renamed: "String.Encoding.windowsCP1253")
public var NSWindowsCP1253StringEncoding: String.Encoding {
  return String.Encoding.windowsCP1253
}
@available(*, unavailable, renamed: "String.Encoding.windowsCP1254")
public var NSWindowsCP1254StringEncoding: String.Encoding {
  return String.Encoding.windowsCP1254
}
@available(*, unavailable, renamed: "String.Encoding.windowsCP1250")
public var NSWindowsCP1250StringEncoding: String.Encoding {
  return String.Encoding.windowsCP1250
}
@available(*, unavailable, renamed: "String.Encoding.iso2022JP")
public var NSISO2022JPStringEncoding: String.Encoding {
  return String.Encoding.iso2022JP
}
@available(*, unavailable, renamed: "String.Encoding.macOSRoman")
public var NSMacOSRomanStringEncoding: String.Encoding {
  return String.Encoding.macOSRoman
}
@available(*, unavailable, renamed: "String.Encoding.utf16")
public var NSUTF16StringEncoding: String.Encoding {
  return String.Encoding.utf16
}
@available(*, unavailable, renamed: "String.Encoding.utf16BigEndian")
public var NSUTF16BigEndianStringEncoding: String.Encoding {
  return String.Encoding.utf16BigEndian
}
@available(*, unavailable, renamed: "String.Encoding.utf16LittleEndian")
public var NSUTF16LittleEndianStringEncoding: String.Encoding {
  return String.Encoding.utf16LittleEndian
}
@available(*, unavailable, renamed: "String.Encoding.utf32")
public var NSUTF32StringEncoding: String.Encoding {
  return String.Encoding.utf32
}
@available(*, unavailable, renamed: "String.Encoding.utf32BigEndian")
public var NSUTF32BigEndianStringEncoding: String.Encoding {
  return String.Encoding.utf32BigEndian
}
@available(*, unavailable, renamed: "String.Encoding.utf32LittleEndian")
public var NSUTF32LittleEndianStringEncoding: String.Encoding {
  return String.Encoding.utf32LittleEndian
}
