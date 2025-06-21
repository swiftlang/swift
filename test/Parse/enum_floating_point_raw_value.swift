// RUN: %target-typecheck-verify-swift

// FIXME: this test only passes on platforms which have Float80.
// <rdar://problem/19508460> Floating point enum raw values are not portable

// REQUIRES: CPU=i386 || CPU=x86_64

// Windows and Android do not support FP80
// UNSUPPORTED: OS=windows-msvc, OS=linux-android

enum RawTypeWithFloatValues : Float { // expected-error {{'RawTypeWithFloatValues' declares raw type 'Float', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{add stubs for conformance}}
  case Northrup = 1.5
  case Overton // expected-error {{enum case must declare a raw value when the preceding raw value is not an integer}}
  case Pettygrove = 2.25
}

enum RawTypeWithRepeatValues2 : Double {
  case Vaughn = 22   // expected-note {{raw value previously used here}}
  case Wilson = 22.0 // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValues3 : Double {
  // 2^63-1
  case Vaughn = 9223372036854775807   // expected-note {{raw value previously used here}}
  case Wilson = 9223372036854775807.0 // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValues4 : Double {
  // 2^64-1
  case Vaughn = 18446744073709551615   // expected-note {{raw value previously used here}}
  case Wilson = 18446744073709551615.0 // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValues5 : Double {
  // FIXME: should reject.
  // 2^65-1
  case Vaughn = 36893488147419103231
  case Wilson = 36893488147419103231.0
}

enum RawTypeWithRepeatValues6 : Double {
  // FIXME: should reject.
  // 2^127-1
  case Vaughn = 170141183460469231731687303715884105727
  case Wilson = 170141183460469231731687303715884105727.0
}

enum RawTypeWithRepeatValues7 : Double {
  // FIXME: should reject.
  // 2^128-1
  case Vaughn = 340282366920938463463374607431768211455
  case Wilson = 340282366920938463463374607431768211455.0
}

enum RawTypeWithNonRepeatValues : Double {
  case SantaClara = 3.7
  case SanFernando = 7.4
  case SanAntonio = -3.7
  case SanCarlos = -7.4
}

enum RawTypeWithRepeatValuesAutoInc : Double {
  case Vaughn = 22 // expected-note {{raw value auto-incremented from here}}
  case Wilson    // expected-note {{raw value previously used here}}
  case Yeon = 23 // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValuesAutoInc2 : Double {
  case Vaughn = 23 // expected-note {{raw value previously used here}}
  case Wilson = 22 // expected-note {{raw value auto-incremented from here}}
  case Yeon // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValuesAutoInc3 : Double {
  case Vaughn // expected-note {{raw value implicitly auto-incremented from zero}}
  case Wilson // expected-note {{raw value previously used here}}
  case Yeon = 1 // expected-error {{raw value for enum case is not unique}}
}

// rdar://problem/22476643
public protocol RawValueA: RawRepresentable
{
  var rawValue: Double { get }
}

enum RawValueATest: Double, RawValueA {
  case A, B
}

public protocol RawValueB
{
  var rawValue: Double { get }
}

enum RawValueBTest: Double, RawValueB {
  case A, B
}
