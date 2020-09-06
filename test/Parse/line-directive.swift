// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -c %s 2>&1 | %FileCheck %s

let x = 0 // We need this because of the #sourceLocation-ends-with-a-newline requirement.

#sourceLocation()
x // expected-error {{parameterless closing #sourceLocation() directive without prior opening #sourceLocation(file:,line:) directive}}

#sourceLocation(file: "x", line: 0) // expected-error{{the line number needs to be greater}}

#sourceLocation(file: "x", line: -1) // expected-error{{expected starting line number}}

#sourceLocation(file: "x", line: 1.5) // expected-error{{expected starting line number}}

#sourceLocation(file: x.swift, line: 1) // expected-error{{expected filename string literal}}

#sourceLocation(file: "x.swift", line: 42)
x x ; // should be ignored by expected_error because it is in a different file
x
#sourceLocation()
_ = x
x x // expected-error{{consecutive statements}} {{2-2=;}}
// expected-warning @-1 2 {{unused}}

// rdar://19582475
public struct S { // expected-note{{in declaration of 'S'}}
// expected-error@+1{{expected declaration}}
/ ###line 25 "line-directive.swift"
}
// expected-error@+1{{#line directive was renamed to #sourceLocation}}
#line 32000 "troops_on_the_water"

#sourceLocation()

// expected-error@+1 {{expected expression}}
try #sourceLocation(file: "try.swift", line: 100)
#sourceLocation()

// expected-error@+3 {{expected statement}}
// expected-error@+2 {{#line directive was renamed to #sourceLocation}}
LABEL:
#line 200 "labeled.swift"
#sourceLocation()

class C {
#sourceLocation(file: "sr5242.swift", line: 100)
    func foo() {}
    let bar = 12
#sourceLocation(file: "sr5242.swift", line: 200)
}
enum E {
#sourceLocation(file: "sr5242.swift", line: 300)
    case A, B
    case C, D
#sourceLocation()
}

#sourceLocation(file: "sr8772.swift", line: 400)
2., 3
// CHECK: sr8772.swift:400:2: error: expected member name following '.'
// CHECK: sr8772.swift:400:3: error: consecutive statements on a line must be separated by ';'
// CHECK: sr8772.swift:400:3: error: expected expression
