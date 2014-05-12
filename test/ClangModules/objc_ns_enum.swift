// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s -verify
// -- Check that we can successfully round-trip.
// RUN: %swift %clang-importer-sdk -emit-ir -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s >/dev/null

import Foundation
import user_objc

var mince = NSRuncingMode.Mince
var quince = NSRuncingMode.Quince

var rawMince: UInt = NSRuncingMode.Mince.toRaw()
var rawFoo: CInt = NSUnderlyingType.Foo.toRaw()
var rawNegativeOne: CUnsignedInt
  = NSUnsignedUnderlyingTypeNegativeValue.NegativeOne.toRaw()

var rawWordBreakA: Int = NSPrefixWordBreak.Banjo.toRaw()
var rawWordBreakB: Int = NSPrefixWordBreak.Bandana.toRaw()

var rawWordBreak2A: Int = NSPrefixWordBreak2.BreakBarBas.toRaw()
var rawWordBreak2B: Int = NSPrefixWordBreak2.BreakBareBass.toRaw()

var rawWordBreak3A: Int = NSPrefixWordBreak3.Break1Bob.toRaw()
var rawWordBreak3B: Int = NSPrefixWordBreak3.Break1Ben.toRaw()

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
