// RUN: %swift -parse -verify %s

oneof Boolish {
  case falsy
  case truthy

  constructor() { this = .falsy }
}

oneof Optional<T> {
  case None
  case Just(T)
}

// Cases are excluded from oneofs.
case FloatingCase // expected-error{{oneof 'case' is not allowed outside of a oneof}}

struct SomeStruct {
  case StructCase // expected-error{{oneof 'case' is not allowed outside of a oneof}}
}

class SomeClass {
  case ClassCase // expected-error{{oneof 'case' is not allowed outside of a oneof}}
}

// Recover when a switch 'case' label is spelled inside a oneof (or outside).
oneof SwitchEnvy {
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

oneof HasMethodsPropertiesAndCtors {
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

oneof ImproperlyHasIVars {
  case Flopsy
  case Mopsy

  var ivar : Int // expected-error{{'var' declarations without getter/setter not allowed here}}
}
