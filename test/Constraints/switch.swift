// RUN: %swift %s -parse -verify

func [infix] =~ (a:Int, b:Int) -> Bool { return a == b }
func [infix] =~ (a:Int, b:IntEnumeratorType) -> Bool {
  return b.min <= a && a < b.max
}

struct MatchDoesNotCastToBool {}

func [infix] =~ (a:Int, b:MatchDoesNotCastToBool) -> String {
  return "false"
}

func typeCheckSuccess(x:Int) {
  switch x {
  case 0:
  case 1, 2, 3:
  case 9..12:
  }
}

func scope(x:Int) {
  switch x {
  case 0:
    var a = 0
  case 1:
    var a = "zero"
  default:
    ++a // expected-error{{unresolved identifier 'a'}}
  }
}
func multipleDefaults(x:Int) {
  switch x {
  default: // expected-note{{previous 'default' here}}
  default: // expected-error{{multiple 'default' branches in 'switch'}}
  }
}

func typeCheckFailure(x:Int) {
  switch x {
  case 0:
  case "one": // expected-error{{does not type-check}}
  case MatchDoesNotCastToBool(): // expected-error{{'String' has no member named 'getLogicValue'}}
  }
}
