// RUN: %target-parse-verify-swift

let x = 0 // We need this because of the #line-ends-with-a-newline requirement.

#line
x // expected-error {{parameterless closing #line directive}}

#line 0 "x" // expected-error{{the line number needs to be greater}}

#line -1 "x" // expected-error{{expected starting line number}}

#line 1.5 "x" // expected-error{{expected starting line number}}

#line 1 x.swift // expected-error{{expected filename string literal}}

#line 42 "x.swift"
x x ; // should be ignored by expeted_error because it is in a different file
x
#line
x
x x // expected-error{{consecutive statements}}
