// RUN: %swift %s -verify

func foo() {}
func bar() {}
func bas() {}

//
// If expression as return value
//

func ret() {
  return if true then foo() else bar()
}

//
// If statements
//

if true {
}

if true {
} else {
}

if true {
} else if false {
} else {
}

//
// If expressions
//

if true
  then foo()
  else bar()
if true
  then foo()
  else if false
  then bar()
  else bas()

//
// If statement in expression context error
//

if true
  then foo()
  else if false {} // expected-error{{'if' statement cannot be used inside an expression}}

//
// If expression in statement context error
//

if true {
} else if false
  then 1 // expected-error{{'if...then' expression cannot be used as 'else' clause of 'if' statement}}
  else 0

