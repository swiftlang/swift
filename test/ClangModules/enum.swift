// RUN:%target-swift-frontend(mock-sdk:%clang-importer-sdk) -I %S/Inputs/custom-modules/ -parse %s -verify
// RUN:%target-swift-frontend(mock-sdk:%clang-importer-sdk) -I %S/Inputs/custom-modules/ -D IRGEN -emit-ir %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import user_objc
import ObjCEnumExtras

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

#if !IRGEN
var alias1 = NSAliasesEnum.BySameValue // expected-error{{}}
var alias2 = NSAliasesEnum.ByEquivalentValue // expected-error{{}}
var alias3 = NSAliasesEnum.ByName // expected-error{{}}

var aliasOriginal = NSAliasesEnum.Original

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
var minceAndQuince: NSRuncingOptions = .EnableMince & .EnableQuince
var minceOrQuince: NSRuncingOptions = .EnableMince | .EnableQuince
var noMince: NSRuncingOptions = ~NSRuncingOptions.EnableMince
minceOrQuince &= noMince
minceOrQuince |= minceAndQuince
minceOrQuince ^= .EnableMince

var minceValue: UInt = minceAndQuince.rawValue
var minceFromMask: NSRuncingOptions = NSRuncingOptions(0)

var nothing: NSRuncingOptions = NSRuncingOptions()
var nothing2: NSRuncingOptions = nil
let nothing3: NSRuncingOptions = .allZeros

// Strip leading 'k' in "kConstant".
let calendarUnit: CFCalendarUnit = .Year | .Weekday

// Match various plurals.
let observingOpts: NSKeyValueObservingOptions = .New | .Old
let bluetoothProps: CBCharacteristicProperties = .Write | .WriteWithoutResponse
let buzzFilter: AlertBuzzes = .Funk | .Sosumi

// Match multi-capital acronym.
let bitmapFormat: NSBitmapFormat = .NSAlphaFirstBitmapFormat | .NS32BitBigEndianBitmapFormat;
let bitmapFormatR: NSBitmapFormatReversed = .NSAlphaFirstBitmapFormatR | .NS32BitBigEndianBitmapFormatR;
let bitmapFormat2: NSBitmapFormat2 = .NSU16a | .NSU32a;
let bitmapFormat3: NSBitmapFormat3 = .NSU16b | .NSS32b;
let bitmapFormat4: NSUBitmapFormat4 = .NSU16c | .NSU32c;
let bitmapFormat5: NSABitmapFormat5 = .NSAA16d | .NSAB32d;

// Drop trailing underscores when possible.
let timeFlags: CMTimeFlags = .Valid | .HasBeenRounded
let timeFlags2: CMTimeFlagsWithNumber = ._Valid | ._888

let objcFlags: objc_flags = .taggedPointer | .swiftRefcount


// Conversions
#if !IRGEN
let _ = OpaqueDerived(NamelessInt8Constant) // expected-error {{cannot find an initializer for type 'OpaqueDerived' that accepts an argument list of type '(Int8)'}}
let _ = OpaqueDerived(NamelessInt32Constant) // okay
let _ = OpaqueDerived(NamelessInt64Constant) // expected-error {{cannot find an initializer for type 'OpaqueDerived' that accepts an argument list of type '(Int64)'}}
#endif
let _ = OpaqueDerived(OpaqueInt8Constant)
let _ = OpaqueDerived(OpaqueInt32Constant)
let _ = OpaqueDerived(OpaqueInt64Constant)
let _ = OpaqueDerived(EnumInt8.Constant)
let _ = OpaqueDerived(EnumInt32.Constant)
let _ = OpaqueDerived(EnumInt64.Constant)
let _ = OpaqueDerived(OptionsInt8.Constant)
let _ = OpaqueDerived(OptionsInt32.Constant)
let _ = OpaqueDerived(OptionsInt64.Constant)

#if !IRGEN
let _ = EnumDerived(NamelessInt8Constant) // expected-error {{cannot find an initializer for type 'EnumDerived' that accepts an argument list of type '(Int8)'}}
let _ = EnumDerived(NamelessInt32Constant) // expected-error {{missing argument label 'rawValue:' in call}}
let _ = EnumDerived(NamelessInt64Constant) // expected-error {{cannot find an initializer for type 'EnumDerived' that accepts an argument list of type '(Int64)'}}
#endif
let _ = EnumDerived(OpaqueInt8Constant)
let _ = EnumDerived(OpaqueInt32Constant)
let _ = EnumDerived(OpaqueInt64Constant)
let _ = EnumDerived(EnumInt8.Constant)
let _ = EnumDerived(EnumInt32.Constant)
let _ = EnumDerived(EnumInt64.Constant)
let _ = EnumDerived(OptionsInt8.Constant)
let _ = EnumDerived(OptionsInt32.Constant)
let _ = EnumDerived(OptionsInt64.Constant)

#if !IRGEN
let _ = OptionsDerived(NamelessInt8Constant) // expected-error {{cannot find an initializer for type 'OptionsDerived' that accepts an argument list of type '(Int8)'}}
let _ = OptionsDerived(NamelessInt32Constant) // okay
let _ = OptionsDerived(NamelessInt64Constant) // expected-error {{cannot find an initializer for type 'OptionsDerived' that accepts an argument list of type '(Int64)'}}
#endif
let _ = OptionsDerived(OpaqueInt8Constant)
let _ = OptionsDerived(OpaqueInt32Constant)
let _ = OptionsDerived(OpaqueInt64Constant)
let _ = OptionsDerived(EnumInt8.Constant)
let _ = OptionsDerived(EnumInt32.Constant)
let _ = OptionsDerived(EnumInt64.Constant)
let _ = OptionsDerived(OptionsInt8.Constant)
let _ = OptionsDerived(OptionsInt32.Constant)
let _ = OptionsDerived(OptionsInt64.Constant)

// CHECK-LABEL: define linkonce_odr hidden i32 @_TFVSC13OpaqueDerivedCfMS_FOSC8EnumInt8S_(i8) {
// CHECK: [[RAW:%.+]] = call i8 @_TFOSC8EnumInt8g8rawValueVSs4Int8(i8 %0)
// CHECK: = icmp slt i8 [[RAW]], 0
// CHECK: <label>
// CHECK: [[EXT:%.+]] = sext i8 [[RAW]] to i32
// CHECK: [[NEW:%.+]] = call i32 @_TFVSC13OpaqueDerivedCfMS_FT8rawValueVSs6UInt32_S_(i32 [[EXT]])
// CHECK: ret i32 [[NEW]]
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden i32 @_TFVSC13OpaqueDerivedCfMS_FOSC9EnumInt32S_(i32) {
// CHECK: [[RAW:%.+]] = call i32 @_TFOSC9EnumInt32g8rawValueVSs6UInt32(i32 %0)
// CHECK: [[NEW:%.+]] = call i32 @_TFVSC13OpaqueDerivedCfMS_FT8rawValueVSs6UInt32_S_(i32 [[RAW]])
// CHECK: ret i32 [[NEW]]
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden i32 @_TFVSC13OpaqueDerivedCfMS_FOSC9EnumInt64S_(i64) {
// CHECK: [[RAW:%.+]] = call i64 @_TFOSC9EnumInt64g8rawValueVSs5Int64(i64 %0)
// CHECK: [[TRUNC:%.+]] = trunc i64 [[RAW]] to i32
// CHECK: br i1
// CHECK: <label>
// CHECK: [[NEW:%.+]] = call i32 @_TFVSC13OpaqueDerivedCfMS_FT8rawValueVSs6UInt32_S_(i32 [[TRUNC]])
// CHECK: ret i32 [[NEW]]
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden { i32, i1 } @_TFOSC11EnumDerivedCfMS_FVSC10OpaqueInt8GSQS__(i8) {
// CHECK: = icmp slt i8 %0, 0
// CHECK: <label>
// CHECK: [[EXT:%.+]] = sext i8 %0 to i32
// CHECK: = call { i32, i1 } @_TFOSC11EnumDerivedCfMS_FT8rawValueVSs6UInt32_GSqS__(i32 %6)
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden { i32, i1 } @_TFOSC11EnumDerivedCfMS_FVSC12OpaqueUInt32GSQS__(i32) {
// CHECK: = call { i32, i1 } @_TFOSC11EnumDerivedCfMS_FT8rawValueVSs6UInt32_GSqS__(i32 %0)
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden { i32, i1 } @_TFOSC11EnumDerivedCfMS_FVSC11OpaqueInt64GSQS__(i64) {
// CHECK: [[TRUNC:%.+]] = trunc i64 %0 to i32
// CHECK: br i1
// CHECK: <label>
// CHECK: = call { i32, i1 } @_TFOSC11EnumDerivedCfMS_FT8rawValueVSs6UInt32_GSqS__(i32 [[TRUNC]])
// CHECK: }
