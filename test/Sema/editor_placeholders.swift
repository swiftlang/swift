// RUN: %target-typecheck-verify-swift

func foo(_ x: Int) -> Int {}
func foo(_ x: Float) -> Float {}
func foo<T>(_ t: T) -> T {}

var v = foo(<#T##x: Float##Float#>) // expected-error {{editor placeholder}}
v = "" // expected-error {{cannot assign value of type 'String' to type 'Float'}}

if (true) {
  <#code#> // expected-error {{editor placeholder}}
}

foo(<#T##x: Undeclared##Undeclared#>) // expected-error {{editor placeholder}} expected-error {{cannot find type 'Undeclared' in scope}}

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

<#foo#>(x:)<Int>; // expected-error {{editor placeholder in source file}}

// These are actually raw identifiers
`<#foo#>` // expected-error {{cannot find '<#foo#>' in scope}}
`<#foo#>`(x:)<Int>; // expected-error {{cannot find '<#foo#>(x:)' in scope}}

for x in <#T#> { // expected-error{{editor placeholder in source file}} expected-error{{for-in loop requires '()' to conform to 'Sequence'}}

}

// rdar://problem/49712598 - crash while trying to rank solutions with different kinds of overloads
func test_ambiguity_with_placeholders(pairs: [(rank: Int, count: Int)]) -> Bool {
  return pairs[<#^ARG^#>].count == 2
  // expected-error@-1 {{editor placeholder in source file}}
  // expected-error@-2 {{ambiguous use of 'subscript(_:)'}}
}

let unboundInPlaceholder1: Array<Never> = <#T##Array#> // expected-error{{editor placeholder in source file}}
let unboundInPlaceholder2: Array<Never> = foo(<#T##t: Array##Array<Never>#>) // expected-error{{editor placeholder in source file}}
