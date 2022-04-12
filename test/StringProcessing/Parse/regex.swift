// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex
// REQUIRES: swift_in_compiler

_ = /abc/
_ = #/abc/#
_ = ##/abc/##

func foo<T>(_ x: T...) {}
foo(/abc/, #/abc/#, ##/abc/##)

let arr = [/abc/, #/abc/#, ##/abc/##]

_ = /\w+/.self
_ = #/\w+/#.self
_ = ##/\w+/##.self

_ = /#\/\#\\/
_ = #/#/\/\#\\/#
_ = ##/#|\|\#\\/##

_ = (#/[*/#, #/+]/#, #/.]/#)
// expected-error@-1 {{cannot parse regular expression: quantifier '+' must appear after expression}}
// expected-error@-2 {{cannot parse regular expression: expected ']'}}
