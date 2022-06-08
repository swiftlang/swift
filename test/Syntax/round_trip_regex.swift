// RUN: %empty-directory(%t)
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -fail-on-parse-error > %t/afterRoundtrip.swift
// RUN: diff -u %s %t/afterRoundtrip.swift
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

_ = #/
multiline
/#

_ = #/
double
multiline
/#

_ = #/
\
/#
