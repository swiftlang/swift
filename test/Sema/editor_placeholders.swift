// RUN: %target-typecheck-verify-swift

func foo(_ x: Int) -> Int {}
func foo(_ x: Float) -> Float {}

var v = foo(<#T##x: Float##Float#>) // expected-error {{editor placeholder}}
v = "" // expected-error {{cannot assign value of type 'String' to type 'Float'}}

if (true) {
  <#code#> // expected-error {{editor placeholder}}
}

foo(<#T##x: Undeclared##Undeclared#>) // expected-error {{editor placeholder}} expected-error {{use of undeclared type 'Undeclared'}}

func f(_ n: Int) {}
let a1 = <#T#> // expected-error{{editor placeholder in source file}}
f(a1) // expected-error{{cannot convert value of type '()' to expected argument type 'Int'}}
let a2 = <#T##Int#> // expected-error{{editor placeholder in source file}}
f(a2)
<#T#> // expected-error{{editor placeholder in source file}}

// FIXME: <rdar://problem/22432828> Lexer yields "editor placeholder in source file" error twice when placeholder is first token
_ = <#T##Int#> // expected-error {{editor placeholder in source file}}

f(<#T#> + 1) // expected-error{{editor placeholder in source file}}
f(<#T##Int#>) // expected-error{{editor placeholder in source file}}
f(<#T##String#>) // expected-error{{editor placeholder in source file}} expected-error{{cannot convert value of type 'String' to expected argument type 'Int'}}

for x in <#T#> { // expected-error{{editor placeholder in source file}} expected-error{{type '()' does not conform to protocol 'Sequence'}}
}
