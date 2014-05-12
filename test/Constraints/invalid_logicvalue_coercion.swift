// RUN: %swift -parse %s -verify

class C {}
var c = C()
if c as C { // expected-error{{type 'C' does not conform to protocol 'LogicValue.Protocol'}}
}

if {1} as ()->Int { // expected-error{{type '() -> Int' does not conform to protocol 'LogicValue.Protocol'}}
}
