// RUN: %swift %clang-importer-sdk -target %target-cpu-apple-macosx51 -typecheck %s -verify
// RUN: %swift %clang-importer-sdk -target %target-cpu-apple-macosx52 -typecheck %s -verify

// REQUIRES: OS=macosx
import Foundation
import user_objc

// Ignore deprecated constants in prefix stripping, even if they aren't deprecated /yet/.
let calendarUnits: NSCalendar.Unit = [.era, .year, .calendar]
let calendarUnits2: NSCalendar.Unit = [.NSMonthCalendarUnit, .NSYearCalendarUnit] // expected-error 2 {{unavailable}}
  // ...unless they're all deprecated.
let calendarUnitsDep: NSCalendarUnitDeprecated = [.eraCalendarUnitDeprecated, .yearCalendarUnitDeprecated] // expected-error 2 {{unavailable}}

// rdar://problem/21081557
func pokeRawValue(_ random: SomeRandomEnum) {
  switch (random) {
  case SomeRandomEnum.RawValue // expected-error{{expression pattern of type 'SomeRandomEnum.RawValue.Type' (aka 'Int.Type') cannot match values of type 'SomeRandomEnum'}}
    // expected-error@-1{{expected ':' after 'case'}}
  }
}
