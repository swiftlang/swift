// RUN: %target-typecheck-verify-swift

var x = 0

case 0: // expected-cparser-error{{'case' label can only appear inside a 'switch' statement}} expected-astgen-error {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
var y = 0
default: // expected-error{{'default' label can only appear inside a 'switch' statement}}
var z = 1

fallthrough // expected-error{{'fallthrough' is only allowed inside a switch}}
