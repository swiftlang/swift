// RUN: %swift %s -dump-parse -verify

func [infix] =~ (a:Int, b:Int) -> Bool {}

func foo() {}

func valid_switches(a:Int) {
  switch a {
  }

  switch a {
  case 0:
  }

  switch a {
  default:
  }

  switch a {
  case 0, 1, 2, 3:
  }

  switch a {
  case 0:
  case 1:
  default:
  }

  switch a {
  default:
  case 0:
  case 1:
  }

  switch a {
  case 0:
  default:
  case 1:
  }

  switch a {
  case 0:
    var b = 1
    foo()
    return
  default:
    var b = "one"
    b = "two"
    return
  }

  while true {
    switch a {
    case 0:
      break
    default:
      continue
    }
  }
}

func bad_switches(a:Int) {
  switch a {
    a = 1 // expected-error{{all statements inside a switch must be covered}}
  case 0:
  }

  switch a {
  case: // expected-error{{expected expression for 'case'}}
    a = 1
  }

case 0: // expected-error{{'case' can only appear inside a 'switch' statement}}
  a = 1
default: // expected-error{{'default' can only appear inside a 'switch' statement}}
  a = 2

  switch a {
  )} // expected-error{{expected expression}}

  switch a {
  case 0:
  )} // expected-error{{expected expression}}

  switch a {
  default:
  )} // expected-error{{expected expression}}
}
