// Error cases for literal expressions in integer generic parameter values
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip

let wrongCount1: InlineArray<(2 + 3), Int> = [1, 2, 3]
// expected-error@-1 {{expected '5' elements in inline array literal, but got '3'}}

let wrongCount2: InlineArray<(1 << 2), Int> = [1, 2, 3, 4, 5, 6]
// expected-error@-1 {{expected '4' elements in inline array literal, but got '6'}}

let wrongCount3: [(3 * 2) of Int] = [1, 2, 3]
// expected-error@-1 {{expected '6' elements in inline array literal, but got '3'}}

let typeMismatch1: InlineArray<(2 + 1), Int> = ["a", "b", "c"]
// expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int'}}
// expected-error@-2 {{cannot convert value of type 'String' to expected element type 'Int'}}
// expected-error@-3 {{cannot convert value of type 'String' to expected element type 'Int'}}

let typeMismatch2: [(1 + 1) of String] = [1, 2]
// expected-error@-1 {{cannot convert value of type 'Int' to expected element type 'String'}}
// expected-error@-2 {{cannot convert value of type 'Int' to expected element type 'String'}}

// Sugar syntax with integer as element type
let invalidElement3: [3 of 5] = [1, 2, 3]
// expected-error@-1 {{cannot use value type '5' for generic argument 'Element'}}

func takeExact3<T>(_ arr: InlineArray<3, T>) {}
func takeExact4<T>(_ arr: InlineArray<4, T>) {}
func testTypeMismatch() {
  let arr: InlineArray<(2 + 1), Int> = [1, 2, 3]
  takeExact3(arr)  // OK - both are 3
  takeExact4(arr)  // expected-error {{cannot convert value of type 'InlineArray<3, Int>' to expected argument type 'InlineArray<4, Int>'}}
  // expected-note@-1 {{arguments to generic parameter 'count' ('3' and '4') are expected to be equal}}
}

func testAssignment() {
    let a: InlineArray<(2 + 2), Int> = [1, 2, 3, 4]  // 4 elements
    let b: InlineArray<(1 + 2), Int> = [1, 2, 3]     // 3 elements

    var c: InlineArray<4, Int> = a  // OK
    c = b  // expected-error {{cannot assign value of type 'InlineArray<3, Int>' to type 'InlineArray<4, Int>'}}
  // expected-note@-1 {{arguments to generic parameter 'count' ('3' and '4') are expected to be equal}}
  _ = c
}

// Valid empty array
let zeroResult: InlineArray<(3 - 3), Int> = []

func getCount() -> Int { return 3 }
let nonLiteral2: InlineArray<(getCount()), Int> = [1, 2, 3]
// expected-error@-1 {{generic value must be an integer literal expression}}
// expected-error@-2 {{not supported in a literal expression}}

// Integer literal cannot be used as the Element type
let invalidElement1: InlineArray<3, 5> = [1, 2, 3]
// expected-error@-1 {{cannot use value type '5' for generic argument 'Element'}}

let nestedMatch: InlineArray<2, InlineArray<(1 + 1), Int>> = [[1, 2], [3, 4]]
let nestedMismatch: InlineArray<2, InlineArray<(1 + 1), Int>> = [[1, 2], [3, 4, 5]]
// expected-error@-1 {{cannot convert value of type '[Int]' to expected element type 'InlineArray<2, Int>'}}

let nestedMismatch2: [(1 + 1) of [(2 + 1) of Int]] = [[1, 2, 3], [4, 5]]
// expected-error@-1 {{cannot convert value of type '[Int]' to expected element type '[3 of Int]'}}

let divZero: InlineArray<(1/0), Int>
// expected-error@-1 {{division by zero}}
// expected-error@-2 {{generic value must be an integer literal expression}}

let backwardsSugar = [Int of (1+2)]
// expected-error@-1 {{cannot pass type 'Int' as a value for generic value 'count'}}
// expected-error@-2 {{expected type}}

struct S<each T> {}
S<1, 2, 3>
// expected-error@-1 {{cannot use value type '1' for generic argument 'each T'}}
// expected-error@-2 {{cannot use value type '2' for generic argument 'each T'}}
// expected-error@-3 {{cannot use value type '3' for generic argument 'each T'}}

struct S2<each T> {}
S2<(1 + 3)>
// expected-error@-1 {{cannot use value type '4' for generic argument 'each T'}}

struct V<let N: Int> {}
var num = 1
var _: V<(num)>
// expected-error@-1 {{not supported in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

let letNum: Int
letNum = 1
var _: V<(letNum)>
// expected-error@-1 {{not supported in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

func genNum() -> Int { 1 }
var _: V<(genNum())>
// expected-error@-1 {{not supported in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

struct SelfRef<let N: Int> {}
func + (lhs: SelfRef<(1 + 1)>, rhs: Int) {}
// expected-error@-1 {{circular reference}}
// expected-note@-2 {{while resolving type '(1 + 1)'}}
// expected-note@-3 {{while resolving type 'SelfRef<(1 + 1)>'}}
// expected-note@-4 {{through reference here}}
