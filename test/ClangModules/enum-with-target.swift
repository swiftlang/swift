// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -parse %s -verify
// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.10 -parse %s -verify

// REQUIRES: OS=macosx
import Foundation

// Ignore deprecated constants in prefix stripping, even if they aren't deprecated /yet/.
let calendarUnits: NSCalendarUnit = [.Era, .Year, .Calendar]
let calendarUnits2: NSCalendarUnit = [.NSMonthCalendarUnit, .NSYearCalendarUnit] // expected-error 2 {{unavailable}}
  // ...unless they're all deprecated.
let calendarUnitsDep: NSCalendarUnitDeprecated = [.EraCalendarUnitDeprecated, .YearCalendarUnitDeprecated] // expected-error 2 {{unavailable}}
