// RUN: %target-parse-verify-swift

let x = 0 // We need this because of the #line-ends-with-a-newline requirement.

#setline
x // expected-error {{parameterless closing #line directive}}

#setline 0 "x" // expected-error{{the line number needs to be greater}}

#setline -1 "x" // expected-error{{expected starting line number}}

#setline 1.5 "x" // expected-error{{expected starting line number}}

#setline 1 x.swift // expected-error{{expected filename string literal}}

#setline 42 "x.swift"
x x ; // should be ignored by expected_error because it is in a different file
x
#setline
x
x x // expected-error{{consecutive statements}} {{2-2=;}}

// rdar://19582475
public struct S { // expected-note{{in declaration of 'S'}}
// expected-error@+1{{expected declaration}}
/ ###line 25 "line-directive.swift"
}
// expected-warning@+1{{#line directive is deprecated, please use #setline instead}}
#line 32000 "troops_on_the_water"
