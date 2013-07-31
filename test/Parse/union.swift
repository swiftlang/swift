// RUN: %swift -parse -verify %s

union Boolish {
  case falsy
  case truthy

  constructor() { this = .falsy }
}

union Optional<T> {
  case None
  case Just(T)
}

// Cases are excluded from unions.
case FloatingCase // expected-error{{union 'case' is not allowed outside of a union}}

struct SomeStruct {
  case StructCase // expected-error{{union 'case' is not allowed outside of a union}}
}

class SomeClass {
  case ClassCase // expected-error{{union 'case' is not allowed outside of a union}}
}

// Recover when a switch 'case' label is spelled inside a union (or outside).
union SwitchEnvy {
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

union HasMethodsPropertiesAndCtors {
  case TweedleDee
  case TweedleDum

  func method() {}
  func staticMethod() {}

  constructor() {}

  subscript(x:Int) -> Int {
    return 0
  }

  var property : Int {
    return 0
  }
}

union ImproperlyHasIVars {
  case Flopsy
  case Mopsy

  var ivar : Int // expected-error{{'var' declarations without getter/setter not allowed here}}
}
