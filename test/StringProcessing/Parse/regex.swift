// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_swift_parser

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
// expected-error@-1:16 {{cannot parse regular expression: quantifier '+' must appear after expression}}
// expected-error@-2:10 {{cannot parse regular expression: expected ']'}}
