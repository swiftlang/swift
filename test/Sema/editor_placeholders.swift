// RUN: %target-parse-verify-swift

func foo(x: Int) -> Int {}
func foo(x: Float) -> Float {}

var v = foo(<#T##x: Float##Float#>) // expected-error {{editor placeholder}}
v = "" // expected-error {{cannot assign a value of type 'String' to a value of type 'Float'}}

if (true) {
  <#code#> // expected-error {{editor placeholder}}
}

foo(<#T##x: Undeclared##Undeclared#>) // expected-error {{editor placeholder}} expected-error {{use of undeclared type 'Undeclared'}}
