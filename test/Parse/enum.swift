// RUN: %swift -parse -verify %s

enum Empty {}

enum Boolish {
  case falsy
  case truthy

  init() { self = .falsy }
}

var b = Boolish.falsy
b = .truthy

enum Optionable<T> {
  case Nought
  case Mere(T)
}

var o = Optionable<Int>.Nought
o = .Mere(0)

enum Color { case Red, Green, Grayscale(Int), Blue }

var c = Color.Red
c = .Green
c = .Grayscale(255)
c = .Blue

// Cases are excluded from non-enums.
case FloatingCase // expected-error{{enum 'case' is not allowed outside of an enum}}

struct SomeStruct {
  case StructCase // expected-error{{enum 'case' is not allowed outside of an enum}}
}

class SomeClass {
  case ClassCase // expected-error{{enum 'case' is not allowed outside of an enum}}
}

enum EnumWithExtension1 {
  case A1
}
extension EnumWithExtension1 {
  case A2 // expected-error{{enum 'case' is not allowed outside of an enum}}
}

// Attributes for enum cases.

enum EnumCaseAttributes {
  @xyz case EmptyAttributes  // expected-error {{unknown attribute 'xyz'}}
}

// Recover when a switch 'case' label is spelled inside an enum (or outside).
enum SwitchEnvy {
  case X: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case X(Y): // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case X, Y: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case X where true: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case X(Y), Z(W): // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case X(Y) where true: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case 0: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case _: // expected-error{{'case' label can only appear inside a 'switch' statement}}
  case (_, var x, 0): // expected-error{{'case' label can only appear inside a 'switch' statement}}
}

enum HasMethodsPropertiesAndCtors {
  case TweedleDee
  case TweedleDum

  func method() {}
  func staticMethod() {}

  init() {}

  subscript(x:Int) -> Int {
    return 0
  }

  var property : Int {
    return 0
  }
}

enum ImproperlyHasIVars {
  case Flopsy
  case Mopsy

  var ivar : Int // expected-error{{'var' declarations without getter/setter not allowed here}}
}

// We used to crash on this.  rdar://14678675
enum rdar14678675 {
  case U1,
  case U2 // expected-error{{expected identifier after comma in enum 'case' declaration}}
  case U3
}

enum Recovery1 {
  case: // expected-error {{'case' label can only appear inside a 'switch' statement}} expected-error {{expected pattern}}
}
enum Recovery2 {
  case UE1: // expected-error {{'case' label can only appear inside a 'switch' statement}}
}
enum Recovery3 {
  case UE2(): // expected-error {{'case' label can only appear inside a 'switch' statement}}
}
enum Recovery4 {
  case Self Self // expected-error {{expected identifier in enum case declaration}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}}
}

enum RawTypeEmpty : Int {} // expected-error {{an enum with no cases cannot declare a raw type}}

enum RawType : Int {
  case Ankeny, Burnside
}

enum MultiRawType : Int64, Int32 { // expected-error {{multiple enum raw types 'Int64' and 'Int32'}}
  case Couch, Davis
}

protocol RawTypeNotFirstProtocol {}
enum RawTypeNotFirst : RawTypeNotFirstProtocol, Int { // expected-error {{raw type 'Int' must appear first in the enum inheritance clause}}
  case E
}

enum RawTypeNotLiteralConvertible : Array<Int> { // expected-error {{raw type 'Array<Int>' is not convertible from any literal}}
  case Ladd, Elliott, Sixteenth, Harrison // expected-error {{enum cases require explicit raw values when the raw type is not integer literal convertible}}
}

enum RawTypeCircularityA : RawTypeCircularityB, IntegerLiteralConvertible { // expected-error {{circular enum raw types 'RawTypeCircularityA' -> 'RawTypeCircularityB' -> 'RawTypeCircularityA'}} FIXME: expected-error{{RawRepresentable}}
  case Morrison, Belmont, Madison, Hawthorne

  static func convertFromIntegerLiteral(value: Int) -> RawTypeCircularityA {
    return .Morrison
  }
}

enum RawTypeCircularityB : RawTypeCircularityA, IntegerLiteralConvertible { // expected-note {{enum 'RawTypeCircularityB' declared here}} 
  case Willamette, Columbia, Sandy, Multnomah

  static func convertFromIntegerLiteral(value: Int) -> RawTypeCircularityB {
    return .Willamette
  }
}

enum RawTypeNotIntegerLiteralConvertible : String {
  case Everett // expected-error {{enum cases require explicit raw values when the raw type is not integer literal convertible}}
  case Flanders
}

enum RawTypeWithIntValues : Int {
  case Glisan = 17, Hoyt = 219, Irving, Johnson = 97209
}

enum RawTypeWithNegativeValues : Int {
  case Glisan = -17, Hoyt = -219, Irving, Johnson = -97209
  case AutoIncAcrossZero = -1, Zero, One
}

enum RawTypeWithUnicodeScalarValues : UnicodeScalar {
  case Kearney = "K"
  case Lovejoy // expected-error {{enum case must declare a raw value when the preceding raw value is not an integer}}
  case Marshall = "M"
}

enum RawTypeWithCharacterValues : Character {
  case First = "い"
  case Second // expected-error {{enum case must declare a raw value when the preceding raw value is not an integer}}
  case Third = "は"
}

enum RawTypeWithCharacterValues_Error1 : Character {
  case First = "abc" // expected-error {{cannot convert the expression's type 'String' to type 'Character'}}
}

enum RawTypeWithFloatValues : Float {
  case Northrup = 1.5
  case Overton // expected-error {{enum case must declare a raw value when the preceding raw value is not an integer}}
  case Pettygrove = 2.25
}

enum RawTypeWithStringValues : String {
  case Quimby = "Lucky Lab"
  case Raleigh // expected-error {{enum case must declare a raw value when the preceding raw value is not an integer}}
  case Savier = "McMenamin's", Thurman = "Kenny and Zuke's"
}

enum RawValuesWithoutRawType {
  case Upshur = 22 // expected-error {{enum case cannot have a raw value if the enum does not have a raw type}}
}

enum RawTypeWithRepeatValues : Int {
  case Vaughn = 22 // expected-note {{raw value previously used here}}
  case Wilson = 22 // expected-error {{raw value for enum case is not unique}}
}

enum RawTypeWithRepeatValues2 : Double {
  case Vaughn = 22   // expected-note {{raw value previously used here}}
  case Wilson = 22.0 // expected-error {{raw value for enum case is not unique}}
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

enum NonliteralRawValue : Int {
  case Yeon = 100 + 20 + 3 // expected-error {{raw value for enum case must be a literal}}
}

enum RawTypeWithPayload : Int { // expected-note {{declared raw type 'Int' here}} expected-note {{declared raw type 'Int' here}}
  case Powell(Int) // expected-error {{enum with raw type cannot have cases with arguments}}
  case Terwilliger(Int) = 17 // expected-error {{enum with raw type cannot have cases with arguments}}
}

enum RawTypeMismatch : Int {
  case Barbur = "foo" // expected-error {{}}
}

enum DuplicateMembers1 {
  case Foo // expected-note {{previous definition of 'Foo' is here}}
  case Foo // expected-error {{duplicate definition of enum element}}
}

enum DuplicateMembers2 {
  case Foo, Bar // expected-note {{previous definition of 'Foo' is here}} expected-note {{previous definition of 'Bar' is here}}
  case Foo // expected-error {{duplicate definition of enum element}}
  case Bar // expected-error {{duplicate definition of enum element}}
}

enum DuplicateMembers3 {
  case Foo // expected-note {{previous definition of 'Foo' is here}}
  case Foo(Int) // expected-error {{duplicate definition of enum element}}
}

enum DuplicateMembers4 : Int {
  case Foo = 1 // expected-note {{previous definition of 'Foo' is here}}
  case Foo = 2 // expected-error {{duplicate definition of enum element}}
}

enum DuplicateMembers5 : Int {
  case Foo = 1 // expected-note {{previous definition of 'Foo' is here}}
  case Foo = 1 + 1 // expected-error {{duplicate definition of enum element}} expected-error {{raw value for enum case must be a literal}}
}

enum DuplicateMembers6 {
  case Foo // expected-note 2{{previous definition of 'Foo' is here}}
  case Foo // expected-error {{duplicate definition of enum element}}
  case Foo // expected-error {{duplicate definition of enum element}}
}

