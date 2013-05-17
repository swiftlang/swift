// RUN: %swift -parse %s -verify

// Note: tests parser error recovery for closure expressions.

func missing_close_pipe() {
  var f1 : () -> Int = {| -> Int 1} // expected-error{{expected '|' to finish closure argument list}}{{26-26=|}} expected-note{{to match this opening '|'}}
  var f2 : (Int) -> Int = {|x -> Int x} // expected-error{{expected '|' to finish closure argument list}}{{30-30=|}} expected-note{{to match this opening '|'}}
}

func missing_comma() {
  var f1 : (Int, Int) -> Int = {|x : Int y| x + y} // expected-error{{missing comma between patterns}}{{41-41=,}}
}

// FIXME: 'missing newline' warning is really lame here.
var f1 : (Int) -> Int = {|x| x; // expected-note{{to match this opening '{'}}
// expected-error{{expected '}' at end of closure}} expected-warning{{missing newline}}