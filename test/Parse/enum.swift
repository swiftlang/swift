// RUN: %swift -parse -verify %s

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

// Cases are excluded from enums.
case FloatingCase // expected-error{{enum 'case' is not allowed outside of an enum}}

struct SomeStruct {
  case StructCase // expected-error{{enum 'case' is not allowed outside of an enum}}
}

class SomeClass {
  case ClassCase // expected-error{{enum 'case' is not allowed outside of an enum}}
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

enum MissingReturnTypeInCase1 {
  case UE1() -> // expected-error {{expected type after '->' in 'case'}}
}
