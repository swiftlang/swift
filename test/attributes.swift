// RUN: %swift %s -verify


func [unknown] f0() {} // expected-error{{unknown attribute 'unknown'}}
func [.] f1() {} // expected-error{{expected an attribute name}}

oneof binary { Zero, One }

// Test infix_left attribute parsing
func [infix_left=300] +(binary, binary) {} // expected-error{{precedence '300' is not an integer between 0 and 255}}
func [infix_left=10,infix_left=20] ++(binary, binary) {} // expected-error{{duplicate 'infix_left' attribute}}
func [infix_left=right] +++(binary, binary) {} // expected-error{{expected precedence number in 'infix_left' attribute}}
