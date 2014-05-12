// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s -strict-keyword-arguments -verify
// -- Check that we can successfully round-trip.
// RUN: %swift %clang-importer-sdk -emit-ir -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s >/dev/null

import Foundation

var withMince: NSRuncingOptions = .EnableMince
var withQuince: NSRuncingOptions = .EnableQuince

// When there is a single enum constant, compare it against the type name to
// derive the namespaced name.
var singleValue: NSSingleOptions = .Value

// Check RawOptionSet conformance.
var minceAndQuince: NSRuncingOptions = .EnableMince & .EnableQuince
var minceOrQuince: NSRuncingOptions = .EnableMince | .EnableQuince
var noMince: NSRuncingOptions = ~NSRuncingOptions.EnableMince

var minceValue: UInt = minceAndQuince.toRaw()
var minceFromMask: NSRuncingOptions = .fromMask(0)

var nothing: NSRuncingOptions = NSRuncingOptions()
var nothing2: NSRuncingOptions = nil

if noMince {}

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
