// RUN: %swift -parse -verify %s

Array.map // expected-error{{reference to generic method 'map' is ambiguous without more context}}

let a = [1, 2, 3]
a.map // expected-error{{reference to generic method 'map' is ambiguous without more context}}

