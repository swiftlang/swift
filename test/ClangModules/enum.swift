// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify
// -- Check that we can successfully round-trip.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -D IRGEN -emit-ir %s >/dev/null

// REQUIRES: objc_interop

import Foundation
import user_objc

// NS_ENUM
var mince = NSRuncingMode.Mince
var quince = NSRuncingMode.Quince

var rawMince: UInt = NSRuncingMode.Mince.rawValue
var rawFoo: CInt = NSUnderlyingType.Foo.rawValue
var rawNegativeOne: CUnsignedInt
  = NSUnsignedUnderlyingTypeNegativeValue.NegativeOne.rawValue

var rawWordBreakA: Int = NSPrefixWordBreak.Banjo.rawValue
var rawWordBreakB: Int = NSPrefixWordBreak.Bandana.rawValue

var rawWordBreak2A: Int = NSPrefixWordBreak2.BreakBarBas.rawValue
var rawWordBreak2B: Int = NSPrefixWordBreak2.BreakBareBass.rawValue

var rawWordBreak3A: Int = NSPrefixWordBreak3.Break1Bob.rawValue
var rawWordBreak3B: Int = NSPrefixWordBreak3.Break1Ben.rawValue

var singleConstant = NSSingleConstantEnum.Value

var myCoolWaterMelon = MyCoolEnum.WaterMelon

var hashMince: Int = NSRuncingMode.Mince.hashValue
if NSRuncingMode.Mince != .Quince { }

var numberBehavior: NSNumberFormatterBehavior = .BehaviorDefault
numberBehavior = .Behavior10_4
var postingStyle: NSPostingStyle = .PostWhenIdle
postingStyle = .PostASAP

func handler(formatter: NSByteCountFormatter) {
	// Ensure that the Equality protocol is properly added to an
	// imported ObjC enum type before the type is referenced by name
    if (formatter.countStyle == .File) {}
}

// Unfortunate consequence of treating runs of capitals as a single word.
// See <rdar://problem/16768954>.
var pathStyle: CFURLPathStyle = .CFURLPOSIXPathStyle
pathStyle = .CFURLWindowsPathStyle
var URLOrUTI: CFURLOrUTI = .CFURLKind
URLOrUTI = .CFUTIKind

let magnitude: Magnitude = .k2
let magnitude2: MagnitudeWords = .Two

let objcABI: objc_abi = .v2
let underscoreSuffix: ALL_CAPS_ENUM = .ENUM_CASE_ONE
let underscoreSuffix2: ALL_CAPS_ENUM2 = .CASE_TWO

var alias1 = NSAliasesEnum.BySameValue
var alias2 = NSAliasesEnum.ByEquivalentValue
var alias3 = NSAliasesEnum.ByName
var aliasOriginal = NSAliasesEnum.Original

switch aliasOriginal {
case .Original:
  break
case .DifferentValue:
  break
}
switch aliasOriginal {
case .Original:
  break
default:
  break
}

switch aliasOriginal {
case .BySameValue:
  break
case .DifferentValue:
  break
}
switch aliasOriginal {
case .BySameValue:
  break
default:
  break
}

switch aliasOriginal {
case NSAliasesEnum.BySameValue:
  break
case NSAliasesEnum.DifferentValue:
  break
}

extension NSAliasesEnum {
  func test() {
    switch aliasOriginal {
    case BySameValue:
      break
    case DifferentValue:
      break
    }
  }
}

// Test NS_SWIFT_NAME:
NSXMLNodeKind.DTDKind == .InvalidKind

NSPrefixWordBreakCustom.ProblemCase == .DeprecatedGoodCase // expected-warning {{deprecated}}

NSPrefixWordBreak2Custom.ProblemCase == .GoodCase
NSPrefixWordBreak2Custom.ProblemCase == .PrefixWordBreak2DeprecatedBadCase // expected-warning {{deprecated}}
NSPrefixWordBreak2Custom.ProblemCase == .DeprecatedGoodCase // expected-warning {{deprecated}}

NSPrefixWordBreakReversedCustom.ProblemCase == .DeprecatedGoodCase // expected-warning {{deprecated}}

NSPrefixWordBreakReorderedCustom.ProblemCase == .GoodCase
NSPrefixWordBreakReorderedCustom.ProblemCase == .PrefixWordBreakReorderedDeprecatedBadCase // expected-warning {{deprecated}}
NSPrefixWordBreakReorderedCustom.ProblemCase == .DeprecatedGoodCase // expected-warning {{deprecated}}

NSPrefixWordBreakReordered2Custom.ProblemCase == .GoodCase
NSPrefixWordBreakReordered2Custom.ProblemCase == .PrefixWordBreakReordered2DeprecatedBadCase // expected-warning {{deprecated}}
NSPrefixWordBreakReordered2Custom.ProblemCase == .DeprecatedGoodCase // expected-warning {{deprecated}}

NSSwiftNameAllTheThings.Foo == .Bar
_ = NSSwiftNameBad.`class`


#if !IRGEN
var qualifiedName = NSRuncingMode.Mince
var topLevelCaseName = NSRuncingMince // expected-error{{}}
#endif

// NS_OPTIONS
var withMince: NSRuncingOptions = .EnableMince
var withQuince: NSRuncingOptions = .EnableQuince

// When there is a single enum constant, compare it against the type name to
// derive the namespaced name.
var singleValue: NSSingleOptions = .Value

// Check RawOptionSetType conformance.
var minceAndQuince: NSRuncingOptions = NSRuncingOptions.EnableMince.intersect(NSRuncingOptions.EnableQuince)
var minceOrQuince: NSRuncingOptions = [.EnableMince, .EnableQuince]
minceOrQuince.intersectInPlace(minceAndQuince)
minceOrQuince.unionInPlace(minceAndQuince)

var minceValue: UInt = minceAndQuince.rawValue
var minceFromMask: NSRuncingOptions = []

// Strip leading 'k' in "kConstant".
let calendarUnit: CFCalendarUnit = [.Year, .Weekday]

// Match various plurals.
let observingOpts: NSKeyValueObservingOptions = [.New, .Old]
let bluetoothProps: CBCharacteristicProperties = [.Write, .WriteWithoutResponse]
let buzzFilter: AlertBuzzes = [.Funk, .Sosumi]

// Match multi-capital acronym.
let bitmapFormat: NSBitmapFormat = [.NSAlphaFirstBitmapFormat, .NS32BitBigEndianBitmapFormat];
let bitmapFormatR: NSBitmapFormatReversed = [.NSAlphaFirstBitmapFormatR, .NS32BitBigEndianBitmapFormatR];
let bitmapFormat2: NSBitmapFormat2  = [.NSU16a  , .NSU32a]
let bitmapFormat3: NSBitmapFormat3  = [.NSU16b  , .NSS32b]
let bitmapFormat4: NSUBitmapFormat4 = [.NSU16c  , .NSU32c]
let bitmapFormat5: NSABitmapFormat5 = [.NSAA16d , .NSAB32d]

// Drop trailing underscores when possible.
let timeFlags: CMTimeFlags = [.Valid , .HasBeenRounded]
let timeFlags2: CMTimeFlagsWithNumber = [._Valid, ._888]

let objcFlags: objc_flags = [.taggedPointer, .swiftRefcount]

let optionsWithSwiftName: NSOptionsAlsoGetSwiftName = .Case
