// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify
// -- Check that we can successfully round-trip.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -D IRGEN -emit-ir %s >/dev/null

// REQUIRES: objc_interop

import Foundation
import user_objc

// NS_ENUM
var mince = RuncingMode.mince
var quince = RuncingMode.quince

var rawMince: UInt = RuncingMode.mince.rawValue
var rawFoo: CInt = UnderlyingType.foo.rawValue
var rawNegativeOne: CUnsignedInt
  = UnsignedUnderlyingTypeNegativeValue.negativeOne.rawValue

var rawWordBreakA: Int = PrefixWordBreak.banjo.rawValue
var rawWordBreakB: Int = PrefixWordBreak.bandana.rawValue

var rawWordBreak2A: Int = PrefixWordBreak2.breakBarBas.rawValue
var rawWordBreak2B: Int = PrefixWordBreak2.breakBareBass.rawValue

var rawWordBreak3A: Int = PrefixWordBreak3.break1Bob.rawValue
var rawWordBreak3B: Int = PrefixWordBreak3.break1Ben.rawValue

var singleConstant = SingleConstantEnum.value

var myCoolWaterMelon = MyCoolEnum.waterMelon

var hashMince: Int = RuncingMode.mince.hashValue
if RuncingMode.mince != .quince { }

var numberBehavior: NumberFormatter.Behavior = .default
numberBehavior = .behavior10_4
var postingStyle: NotificationQueue.PostingStyle = .whenIdle
postingStyle = .asap

func handler(_ formatter: ByteCountFormatter) {
	// Ensure that the Equality protocol is properly added to an
	// imported ObjC enum type before the type is referenced by name
    if (formatter.countStyle == .file) {}
}

// Unfortunate consequence of treating runs of capitals as a single word.
// See <rdar://problem/16768954>.
var pathStyle: CFURLPathStyle = .cfurlposixPathStyle
pathStyle = .cfurlWindowsPathStyle
var URLOrUTI: CFURLOrUTI = .cfurlKind
URLOrUTI = .cfutiKind

let magnitude: Magnitude = .k2
let magnitude2: MagnitudeWords = .two

let objcABI: objc_abi = .v2
let underscoreSuffix: ALL_CAPS_ENUM = .ENUM_CASE_ONE
let underscoreSuffix2: ALL_CAPS_ENUM2 = .CASE_TWO

var alias1 = AliasesEnum.bySameValue
var alias2 = AliasesEnum.byEquivalentValue
var alias3 = AliasesEnum.byName
var aliasOriginal = AliasesEnum.original

switch aliasOriginal {
case .original:
  break
case .differentValue:
  break
}
switch aliasOriginal {
case .original:
  break
default:
  break
}

switch aliasOriginal {
case .bySameValue:
  break
case .differentValue:
  break
}
switch aliasOriginal {
case .bySameValue:
  break
default:
  break
}

switch aliasOriginal {
case AliasesEnum.bySameValue:
  break
case AliasesEnum.differentValue:
  break
}

extension AliasesEnum {
  func test() {
    switch aliasOriginal {
    case .bySameValue:
      break
    case .differentValue:
      break
    }
  }
}

// Test NS_SWIFT_NAME:
_ = XMLNode.Kind.DTDKind == .invalid

_ = PrefixWordBreakCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = PrefixWordBreak2Custom.problemCase == .goodCase
_ = PrefixWordBreak2Custom.problemCase == .PrefixWordBreak2DeprecatedBadCase // expected-warning {{deprecated}}
_ = PrefixWordBreak2Custom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = PrefixWordBreakReversedCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = PrefixWordBreakReorderedCustom.problemCase == .goodCase
_ = PrefixWordBreakReorderedCustom.problemCase == .PrefixWordBreakReorderedDeprecatedBadCase // expected-warning {{deprecated}}
_ = PrefixWordBreakReorderedCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = PrefixWordBreakReordered2Custom.problemCase == .goodCase
_ = PrefixWordBreakReordered2Custom.problemCase == .PrefixWordBreakReordered2DeprecatedBadCase // expected-warning {{deprecated}}
_ = PrefixWordBreakReordered2Custom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = SwiftNameAllTheThings.Foo == .Bar
_ = SwiftNameBad.`class`


#if !IRGEN
var qualifiedName = RuncingMode.mince
var topLevelCaseName = RuncingMince // expected-error{{}}
#endif

// NS_OPTIONS
var withMince: RuncingOptions = .enableMince
var withQuince: RuncingOptions = .enableQuince

// When there is a single enum constant, compare it against the type name to
// derive the namespaced name.
var singleValue: SingleOptions = .value

// Check OptionSet conformance.
var minceAndQuince: RuncingOptions = RuncingOptions.enableMince.intersection(RuncingOptions.enableQuince)
var minceOrQuince: RuncingOptions = [.enableMince, .enableQuince]
minceOrQuince.formIntersection(minceAndQuince)
minceOrQuince.formUnion(minceAndQuince)

var minceValue: UInt = minceAndQuince.rawValue
var minceFromMask: RuncingOptions = []

// Strip leading 'k' in "kConstant".
let calendarUnit: CFCalendarUnit = [.year, .weekday]
// ...unless the next character is a non-identifier.
let curve3D: AU3DMixerAttenuationCurve = .k3DMixerAttenuationCurve_Exponential

// Match various plurals.
let observingOpts: NSKeyValueObservingOptions = [.new, .old]
let bluetoothProps: CBCharacteristicProperties = [.write, .writeWithoutResponse]
let buzzFilter: AlertBuzzes = [.funk, .sosumi]

// Match multi-capital acronym.
let bitmapFormat: BitmapFormat = [.NSAlphaFirstBitmapFormat, .NS32BitBigEndianBitmapFormat];
let bitmapFormatR: BitmapFormatReversed = [.NSAlphaFirstBitmapFormatR, .NS32BitBigEndianBitmapFormatR];
let bitmapFormat2: BitmapFormat2  = [.NSU16a  , .NSU32a]
let bitmapFormat3: BitmapFormat3  = [.NSU16b  , .NSS32b]
let bitmapFormat4: UBitmapFormat4 = [.NSU16c  , .NSU32c]
let bitmapFormat5: ABitmapFormat5 = [.NSAA16d , .NSAB32d]

// Drop trailing underscores when possible.
let timeFlags: CMTimeFlags = [.valid , .hasBeenRounded]
let timeFlags2: CMTimeFlagsWithNumber = [._Valid, ._888]
let audioComponentOpts: AudioComponentInstantiationOptions = [.loadOutOfProcess]
let audioComponentFlags: AudioComponentFlags = [.sandboxSafe]
let audioComponentFlags2: FakeAudioComponentFlags = [.loadOutOfProcess]

let objcFlags: objc_flags = [.taggedPointer, .swiftRefcount]

let optionsWithSwiftName: OptionsAlsoGetSwiftName = .Case

// <rdar://problem/25168818> Don't import None members in NS_OPTIONS types
#if !IRGEN
let _ = RuncingOptions.none // expected-error {{'none' is unavailable: use [] to construct an empty option set}}
#endif
// ...but do if they have a custom name
_ = EmptySet1.default
// ...even if the custom name is the same as the name they would have had
_ = EmptySet2.none
// ...or the original name.
_ = EmptySet3.None
