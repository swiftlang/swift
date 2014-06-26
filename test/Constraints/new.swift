// RUN: %swift -parse -verify %s

let i = 10
new Int[i] // expected-error{{array 'new' has been removed; use array construction instead}}

