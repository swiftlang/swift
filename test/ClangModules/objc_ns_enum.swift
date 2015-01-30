// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify
// -- Check that we can successfully round-trip.
// RUN: %target-swift-frontend %clang-importer-sdk -emit-ir %s >/dev/null

// REQUIRES: objc_interop

import Foundation
import user_objc

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
