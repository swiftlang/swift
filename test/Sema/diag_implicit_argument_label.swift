// RUN: %target-parse-verify-swift -I %S/Inputs -enable-source-import
import diag_implicit_argument_multi_module

// expected-note @+1 {{use '_' to remove the argument label for 'foo'}} {{10-10=_ }}
func foo(bar: Int) -> Int { return bar }

func oneMissing(bar: Int, baz: Int) {}

// expected-note @+1 {{add missing argument label 'bar:'}}
let x = foo(3) // expected-error{{missing argument label 'bar:'}}

let y = someFunction(5) // expected-error {{missing argument label 'explicitFirstParam:' in call}}{{22-22=explicitFirstParam: }}

oneMissing(foobar: 3, 4) //expected-error {{incorrect argument labels in call (have 'foobar:_:', expected 'bar:baz:')}}
