// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -c %s -diagnostic-style llvm >%t.txt 2>&1
// RUN: %FileCheck --input-file %t.txt %s

let x = 0 // We need this because of the #sourceLocation-ends-with-a-newline requirement.

#sourceLocation()
x // expected-error {{parameterless closing #sourceLocation() directive without prior opening #sourceLocation(file:,line:) directive}}

#sourceLocation(file: "x", line: 0) // expected-error{{the line number needs to be greater}}

#sourceLocation(file: "x", line: -1) // expected-error{{expected starting line number}}

#sourceLocation(file: "x", line: 1.5) // expected-error{{expected starting line number}}

#sourceLocation(file: x.swift, line: 1) // expected-error{{expected filename string literal}}

#sourceLocation(file: "x.swift", line: 42)
x x ; // expected-error {{consecutive statements on a line must be separated by ';'}} expected-warning 2 {{expression of type 'Int' is unused}}
// CHECK-DAG: x.swift:42:2: error: consecutive statements on a line must be separated by ';'
x // expected-warning {{expression of type 'Int' is unused}}
// CHECK-DAG: x.swift:44:1: warning: expression of type 'Int' is unused
#sourceLocation()
_ = x
x x // expected-error{{consecutive statements}} {{2-2=;}}
// expected-warning @-1 2 {{unused}}
// CHECK-DAG: line-directive.swift:[[@LINE-2]]:2: error: consecutive statements on a line must be separated by ';'

// rdar://19582475
public struct S {
// expected-error@+8{{expected 'func' keyword in operator function declaration}}
// expected-error@+7{{operator '/' declared in type 'S' must be 'static'}}
// expected-error@+6{{expected '(' in argument list of function declaration}}
// expected-error@+5{{operators must have one or two arguments}}
// expected-error@+4{{member operator '/()' must have at least one argument of type 'S'}}
// expected-error@+3{{expected '{' in body of function declaration}}
// expected-error@+2 {{consecutive declarations on a line must be separated by ';}}
// expected-error@+1 {{expected a macro identifier}}
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

// https://github.com/apple/swift/issues/47817
class C {
#sourceLocation(file: "issue-47817.swift", line: 100)
    func foo() {}
    let bar = 12
#sourceLocation(file: "issue-47817", line: 200)
}
enum E {
#sourceLocation(file: "issue-47817", line: 300)
    case A, B
    case C, D
#sourceLocation()
}

// https://github.com/apple/swift/issues/51280
#sourceLocation(file: "issue-51280.swift", line: 400)
2., 3 // expected-error {{expected member name following '.'}} expected-error {{consecutive statements on a line must be separated by ';'}} expected-error {{expected expression}}
// CHECK-DAG: issue-51280.swift:400:2: error: expected member name following '.'
// CHECK-DAG: issue-51280.swift:400:3: error: consecutive statements on a line must be separated by ';'
// CHECK-DAG: issue-51280.swift:400:3: error: expected expression

// https://github.com/apple/swift/issues/55049
class I55049 {
#sourceLocation(file: "issue-55049.swift", line: 1_000)
    let bar = 12
#sourceLocation(file: "issue-55049.swift", line: 2_000)
}

// expected-error@+1 {{#line directive was renamed to #sourceLocation}}
#line 1_000 "issue-55049.swift"
class I55049_1 {}
