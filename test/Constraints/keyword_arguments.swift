// RUN: %swift -parse %s -verify

func f1(a: Int) { }

// Single extraneous keyword argument
f1(a: 5) // expected-error{{extraneous argument label in call}}{{4-7=}}
