// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -compiler-assertions
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -compiler-assertions 2>&1 | %FileCheck %s
// -- Check that we can successfully round-trip.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -D IRGEN -emit-ir -primary-file %s -compiler-assertions | %FileCheck -check-prefix=CHECK-IR %s

// REQUIRES: objc_interop

// At one point we diagnosed enum case aliases that referred to unavailable 
// cases in their (synthesized) implementation.
// CHECK-NOT: unknown

import Foundation
import user_objc
import enums_using_attributes

// NS_ENUM
var mince = NSRuncingMode.mince
var quince = NSRuncingMode.quince

var rawMince: UInt = NSRuncingMode.mince.rawValue
var rawFoo: CInt = NSUnderlyingType.foo.rawValue
var rawNegativeOne: CUnsignedInt
  = NSUnsignedUnderlyingTypeNegativeValue.negativeOne.rawValue

var rawWordBreakA: Int = NSPrefixWordBreak.banjo.rawValue
var rawWordBreakB: Int = NSPrefixWordBreak.bandana.rawValue

var rawWordBreak2A: Int = NSPrefixWordBreak2.breakBarBas.rawValue
var rawWordBreak2B: Int = NSPrefixWordBreak2.breakBareBass.rawValue

var rawWordBreak3A: Int = NSPrefixWordBreak3.break1Bob.rawValue
var rawWordBreak3B: Int = NSPrefixWordBreak3.break1Ben.rawValue

var singleConstant = NSSingleConstantEnum.value

var myCoolWaterMelon = MyCoolEnum.waterMelon

var hashMince: Int = NSRuncingMode.mince.hashValue
if NSRuncingMode.mince != .quince { }

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

var alias1 = NSAliasesEnum.bySameValue
var alias2 = NSAliasesEnum.byEquivalentValue
var alias3 = NSAliasesEnum.byName
var aliasOriginal = NSAliasesEnum.original

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
default:
  break
}
switch aliasOriginal {
case .bySameValue:
  break
default:
  break
}

switch aliasOriginal {
case NSAliasesEnum.bySameValue:
  break
case NSAliasesEnum.differentValue:
  break
default:
  break
}

extension NSAliasesEnum {
  func test() {
    switch aliasOriginal {
    case .bySameValue:
      break
    case .differentValue:
      break
    default:
      break
    }
  }
}

#if !IRGEN
_ = NSUnavailableAliasesEnum.originalAU
_ = NSUnavailableAliasesEnum.aliasAU // expected-error {{'aliasAU' is unavailable}}
_ = NSUnavailableAliasesEnum.originalUA // expected-error {{'originalUA' is unavailable}}
_ = NSUnavailableAliasesEnum.aliasUA
_ = NSUnavailableAliasesEnum.originalUU // expected-error {{'originalUU' is unavailable}}
_ = NSUnavailableAliasesEnum.aliasUU // expected-error {{'aliasUU' is unavailable}}
#endif

// Test NS_SWIFT_NAME:
_ = XMLNode.Kind.DTDKind == .invalid

_ = NSPrefixWordBreakCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = NSPrefixWordBreak2Custom.problemCase == .goodCase
_ = NSPrefixWordBreak2Custom.problemCase == .PrefixWordBreak2DeprecatedBadCase // expected-warning {{deprecated}}
_ = NSPrefixWordBreak2Custom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = NSPrefixWordBreakReversedCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = NSPrefixWordBreakReorderedCustom.problemCase == .goodCase
_ = NSPrefixWordBreakReorderedCustom.problemCase == .PrefixWordBreakReorderedDeprecatedBadCase // expected-warning {{deprecated}}
_ = NSPrefixWordBreakReorderedCustom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

_ = NSPrefixWordBreakReordered2Custom.problemCase == .goodCase
_ = NSPrefixWordBreakReordered2Custom.problemCase == .PrefixWordBreakReordered2DeprecatedBadCase // expected-warning {{deprecated}}
_ = NSPrefixWordBreakReordered2Custom.problemCase == .deprecatedGoodCase // expected-warning {{deprecated}}

#if !IRGEN
_ = NSPrefixWordBreakForgotToDeprecate.newName1 // expected-error {{type 'NSPrefixWordBreakForgotToDeprecate' has no case 'newName1', but it does have a case named 'PrefixWordBreakNewName1'; if 'newName1' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
_ = NSPrefixWordBreakForgotToDeprecate.newName2 // expected-error {{type 'NSPrefixWordBreakForgotToDeprecate' has no case 'newName2', but it does have a case named 'PrefixWordBreakNewName2'; if 'newName2' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
_ = NSPrefixWordBreakForgotToDeprecate.noMatches // expected-error {{type 'NSPrefixWordBreakForgotToDeprecate' has no member 'noMatches'}}

_ = NSPrefixWordBreakInvalidWord.foo // expected-error {{type 'NSPrefixWordBreakInvalidWord' has no case 'foo', but it does have a case named 'BreakFoo'; if 'foo' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
_ = NSPrefixWordBreakInvalidWord.bar // expected-error {{type 'NSPrefixWordBreakInvalidWord' has no case 'bar', but it does have a case named 'BreakBar'; if 'bar' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
_ = NSPrefixWordBreakInvalidWord.noMatches // expected-error {{type 'NSPrefixWordBreakInvalidWord' has no member 'noMatches'}}

_ = NSPrefixWordBreakSuffixTieBreakers.forTest1 // expected-error {{Better}}
_ = NSPrefixWordBreakSuffixTieBreakers.forTest2 // expected-error {{Better}}
_ = NSPrefixWordBreakSuffixTieBreakers.forTest3 // expected-error {{Better}}
_ = NSPrefixWordBreakSuffixTieBreakers.forTest4 // expected-error {{Better}}
_ = NSPrefixWordBreakSuffixTieBreakers.forTest5 // expected-error {{Better}}

_ = [
  .newOption1,  // expected-error {{type 'NSPrefixWordBreakOptions.ArrayLiteralElement' (aka 'NSPrefixWordBreakOptions') has no case 'newOption1', but it does have a case named 'prefixWordBreakNewOption1'; if 'newOption1' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
  .newOption2,  // expected-error {{type 'NSPrefixWordBreakOptions.ArrayLiteralElement' (aka 'NSPrefixWordBreakOptions') has no case 'newOption2', but it does have a case named 'prefixWordBreakNewOption2'; if 'newOption2' worked before, a recent change to the underlying C enum may have affected how prefixes are stripped from its case names}}
] as NSPrefixWordBreakOptions
#endif

_ = NSSwiftNameAllTheThings.Foo == .Bar
_ = NSSwiftNameBad.`class`


#if !IRGEN
var qualifiedName = NSRuncingMode.mince
var topLevelCaseName = RuncingMince // expected-error{{}}
#endif

let _: EnumViaAttribute = .first
let _: CFEnumWithAttr = .first

// NS_OPTIONS
var withMince: NSRuncingOptions = .enableMince
var withQuince: NSRuncingOptions = .enableQuince

// When there is a single enum constant, compare it against the type name to
// derive the namespaced name.
var singleValue: NSSingleOptions = .value

// Check OptionSet conformance.
var minceAndQuince: NSRuncingOptions = NSRuncingOptions.enableMince.intersection(NSRuncingOptions.enableQuince)
var minceOrQuince: NSRuncingOptions = [.enableMince, .enableQuince]
minceOrQuince.formIntersection(minceAndQuince)
minceOrQuince.formUnion(minceAndQuince)

var minceValue: UInt = minceAndQuince.rawValue
var minceFromMask: NSRuncingOptions = []

// Strip leading 'k' in "kConstant".
let calendarUnit: CFCalendarUnit = [.year, .weekday]
// ...unless the next character is a non-identifier.
let curve3D: AU3DMixerAttenuationCurve = .k3DMixerAttenuationCurve_Exponential

// Match various plurals.
let observingOpts: NSKeyValueObservingOptions = [.new, .old]
let bluetoothProps: CBCharacteristicProperties = [.write, .writeWithoutResponse]
let buzzFilter: AlertBuzzes = [.funk, .sosumi]

// Match multi-capital acronym.
let bitmapFormat: NSBitmapFormat = [.NSAlphaFirstBitmapFormat, .NS32BitBigEndianBitmapFormat];
let bitmapFormatR: NSBitmapFormatReversed = [.NSAlphaFirstBitmapFormatR, .NS32BitBigEndianBitmapFormatR];
let bitmapFormat2: NSBitmapFormat2  = [.NSU16a  , .NSU32a]
let bitmapFormat3: NSBitmapFormat3  = [.NSU16b  , .NSS32b]
let bitmapFormat4: NSUBitmapFormat4 = [.NSU16c  , .NSU32c]
let bitmapFormat5: NSABitmapFormat5 = [.NSAA16d , .NSAB32d]

// Drop trailing underscores when possible.
let timeFlags: CMTimeFlags = [.valid , .hasBeenRounded]
let timeFlags2: CMTimeFlagsWithNumber = [._Valid, ._888]
let audioComponentOpts: AudioComponentInstantiationOptions = [.loadOutOfProcess]
let audioComponentFlags: AudioComponentFlags = [.sandboxSafe]
let audioComponentFlags2: FakeAudioComponentFlags = [.loadOutOfProcess]

let objcFlags: objc_flags = [.taggedPointer, .swiftRefcount]

let optionsWithSwiftName: NSOptionsAlsoGetSwiftName = .Case
let optionsViaAttribute: OptionsViaAttribute = [.first, .second]
let optionsViaAttribute2: CFOptionsWithAttr = [.first]

// <rdar://problem/25168818> Don't import None members in NS_OPTIONS types
#if !IRGEN
let _ = NSRuncingOptions.none // expected-error {{'none' is unavailable: use [] to construct an empty option set}}
#endif
// ...but do if they have a custom name
_ = EmptySet1.default
// ...even if the custom name is the same as the name they would have had
_ = EmptySet2.none
// ...or the original name.
_ = EmptySet3.None

// Just use this type, making sure that its case alias doesn't cause problems.
// rdar://problem/30401506
_ = EnumWithAwkwardDeprecations.normalCase1

#if !IRGEN
let _: UnknownEnumThanksToAPINotes = .first // expected-error {{has no member 'first'}}
let _: UnknownOptionsThanksToAPINotes = .first // expected-error {{has no member 'first'}}
#endif
let _ = UnknownEnumThanksToAPINotesFirst
let _ = UnknownOptionsThanksToAPINotesFirst

// CHECK-IR: $s4enum12testMangling2e12e22e3ySo9EnumByTagV_So0gH7TypedefaSo0gH4BothVtF
func testMangling(e1: EnumByTag, e2: EnumByTypedef, e3: EnumByBoth) {}
