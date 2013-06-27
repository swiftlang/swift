// RUN: %swift -dump-parse -verify %s

var x:Int

switch x {}

switch x {
case 0:
  break
// Multiple patterns per case
case 1, 2, 3:
  continue
// Multiple cases per case block
case 4:
case 5:
  fallthrough
// 'where' guard
case 6..12 where x % 2 == 0:
  x = 1
  x = 2
  x = 3
default:
  x = 1
}

switch x {
  x = 1 // expected-error{{all statements inside a switch must be covered by a 'case' or 'default'}}
default:
  fallthrough
case 0: // expected-error{{additional 'case' blocks cannot appear after the 'default' block of a 'switch'}}
  fallthrough
case 1:
  fallthrough
}

switch x {
default:
  fallthrough
default: // expected-error{{additional 'case' blocks cannot appear after the 'default' block of a 'switch'}}
  fallthrough
}

switch x {
  x = 1 // expected-error{{all statements inside a switch must be covered by a 'case' or 'default'}}
}

switch x {
  x = 1 // expected-error{{all statements inside a switch must be covered by a 'case' or 'default'}}
  x = 2
}

switch x {
default:
case 0: // expected-error{{'default' cannot appear with other 'case' or 'default' labels over the same block}}
  fallthrough
}

switch x {
default:
default: // expected-error{{'default' cannot appear with other 'case' or 'default' labels over the same block}}
  fallthrough
}

switch x {
case 0:
default: // expected-error{{'default' cannot appear with other 'case' or 'default' labels over the same block}}
  fallthrough
}

switch x {
default where x == 0: // expected-error{{'default' cannot be used with a 'where' guard expression}}
  fallthrough
}

switch x {
case 0:
}

switch x {
case 0:
case 1:
}

case 0: // expected-error{{'case' can only appear inside a 'switch' statement}}
var y = 0
default: // expected-error{{'default' can only appear inside a 'switch' statement}}
var z = 1
