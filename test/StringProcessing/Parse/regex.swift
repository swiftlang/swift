// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

_ = #/abc/#
_ = #|abc|#
_ = re'abc'

func foo<T>(_ x: T...) {}
foo(#/abc/#, #|abc|#, re'abc')

let arr = [#/abc/#, #|abc|#, re'abc']

_ = #/\w+/#.self
_ = #|\w+|#.self
_ = re'\w+'.self

_ = #/#/\/\#\\/#
_ = #|#|\|\#\\|#
_ = re're\r\e\'\\'

_ = (#/[*/#, #/+]/#, #/.]/#)
// expected-error@-1 {{cannot parse regular expression: quantifier '+' must appear after expression}}
// expected-error@-2 {{cannot parse regular expression: expected ']'}}

// Make sure we can skip over `'` characters in the regex body.
_ = re'(?'xA0_-y1'x)'
_ = re'(?('xA0_')\')'
_ = re'\'(?('-20'))'
_ = re'\k'+2-1'\''
_ = re'\g'xA0_''
_ = re'(?C'9,3, pg(')'
