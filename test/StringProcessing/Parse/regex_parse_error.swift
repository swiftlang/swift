// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_swift_parser

_ = /(/ // expected-error@:7 {{expected ')'}}
_ = #/(/# // expected-error@:8 {{expected ')'}}

// FIXME: Should be 'group openings'
_ = /)/ // expected-error@:6 {{closing ')' does not balance any groups openings}}
_ = #/)/# // expected-error@:7 {{closing ')' does not balance any groups openings}}

_ = #/\\/''/ // expected-error@:5 {{unterminated regex literal}}
_ = #/\| // expected-error@:5 {{unterminated regex literal}}
_ = #// // expected-error@:5 {{unterminated regex literal}}

_ = #/xy // expected-error@:5 {{unterminated regex literal}}

_ = #/(?/# // expected-error@:9 {{expected group specifier}}
_ = #/(?'/# // expected-error@:10 {{expected group name}}
_ = #/(?'abc/# // expected-error@:13 {{expected '''}}
_ = #/(?'abc /# // expected-error@:13 {{expected '''}}

do {
  _ = #/(?'a
  // expected-error@-1:7 {{unterminated regex literal}}
  // expected-error@-2:13 {{cannot parse regular expression: expected '''}}
}

_ = #/\(?'abc/#

do {
  _ = /\
  /
  // expected-error@-1:3 {{expected expression path in Swift key path}}
}

do {
  _ = #/\
  /#
  // expected-error@-2:7 {{unterminated regex literal}}
  // expected-error@-3:10 {{expected escape sequence}}
  // expected-error@-3:5 {{expected a macro identifier}}
}

func foo<T>(_ x: T, _ y: T) {}
foo(#/(?/#, #/abc/#) // expected-error@:9 {{expected group specifier}}
foo(#/(?C/#, #/abc/#) // expected-error@:10 {{expected ')'}}

foo(#/(?'/#, #/abc/#) // expected-error@:10 {{expected group name}}
