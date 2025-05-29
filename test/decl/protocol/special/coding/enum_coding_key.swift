// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Enums with no raw type conforming to CodingKey should get implicit derived
// conformance of methods.
enum NoRawTypeKey : CodingKey {
    case a, b, c
}

let _ = NoRawTypeKey.a.stringValue
let _ = NoRawTypeKey(stringValue: "a")
let _ = NoRawTypeKey.a.intValue
let _ = NoRawTypeKey(intValue: 0)

// Enums with raw type of String conforming to CodingKey should get implicit
// derived conformance of methods.
enum StringKey : String, CodingKey {
    case a = "A", b, c = "Foo"
}

let _ = StringKey.a.stringValue
let _ = StringKey(stringValue: "A")
let _ = StringKey.a.intValue
let _ = StringKey(intValue: 0)

// Enums with raw type of Int conforming to CodingKey should get implicit
// derived conformance of methods.
enum IntKey : Int, CodingKey {
    case a = 3, b, c = 1
}

let _ = IntKey.a.stringValue
let _ = IntKey(stringValue: "a")
let _ = IntKey.a.intValue
let _ = IntKey(intValue: 3)

// Enums with a different raw value conforming to CodingKey should not get
// implicit derived conformance.
enum Int8Key : Int8, CodingKey { // expected-error {{type 'Int8Key' does not conform to protocol 'CodingKey'}} expected-note {{add stubs for conformance}}
    case a = -1, b = 0, c = 1
}

// Structs conforming to CodingKey should not get implicit derived conformance.
struct StructKey : CodingKey { // expected-error {{type 'StructKey' does not conform to protocol 'CodingKey'}} expected-note {{add stubs for conformance}}
}

// Classes conforming to CodingKey should not get implicit derived conformance.
class ClassKey : CodingKey { //expected-error {{type 'ClassKey' does not conform to protocol 'CodingKey'}} expected-note {{add stubs for conformance}}
}

// Types which are valid for CodingKey derived conformance should not get that
// derivation unless they explicitly conform to CodingKey.
enum X          { case a }
enum Y : String { case a } // expected-note {{property 'rawValue' is implicitly declared}}
enum Z : Int    { case a } // expected-note {{property 'rawValue' is implicitly declared}}

let _ = X.a.stringValue // expected-error {{value of type 'X' has no member 'stringValue'}}
let _ = Y.a.stringValue // expected-error {{value of type 'Y' has no member 'stringValue'}}
let _ = Z.a.stringValue // expected-error {{value of type 'Z' has no member 'stringValue'}}

let _ = X(stringValue: "a") // expected-error {{'X' cannot be constructed because it has no accessible initializers}}
let _ = Y(stringValue: "a") // expected-error {{incorrect argument label in call (have 'stringValue:', expected 'rawValue:')}}
let _ = Z(stringValue: "a") // expected-error {{incorrect argument label in call (have 'stringValue:', expected 'rawValue:')}}
// expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'Int'}}

let _ = X.a.intValue // expected-error {{value of type 'X' has no member 'intValue'}}
let _ = Y.a.intValue // expected-error {{value of type 'Y' has no member 'intValue'; did you mean 'rawValue'?}}
let _ = Z.a.intValue // expected-error {{value of type 'Z' has no member 'intValue'; did you mean 'rawValue'?}}

let _ = X(intValue: 0) // expected-error {{'X' cannot be constructed because it has no accessible initializers}}
let _ = Y(intValue: 0) // expected-error {{incorrect argument label in call (have 'intValue:', expected 'rawValue:')}}
// expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _ = Z(intValue: 0) // expected-error {{incorrect argument label in call (have 'intValue:', expected 'rawValue:')}}

// Types which are valid for CodingKey derived conformance should get derivation
// through extensions.
enum X2          { case a }
enum Y2 : String { case a }
enum Z2 : Int    { case a }
extension X2 : CodingKey {}
extension Y2 : CodingKey {}
extension Z2 : CodingKey {}

let _ = X2.a.stringValue
let _ = Y2.a.stringValue
let _ = Z2.a.stringValue

let _ = X2(stringValue: "a")
let _ = Y2(stringValue: "a")
let _ = Z2(stringValue: "a")

let _ = X2.a.intValue
let _ = Y2.a.intValue
let _ = Z2.a.intValue

let _ = X2(intValue: 0)
let _ = Y2(intValue: 0)
let _ = Z2(intValue: 0)
