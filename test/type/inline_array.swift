// RUN: %target-typecheck-verify-swift -enable-experimental-feature InlineArrayTypeSugar -disable-availability-checking

// REQUIRES: swift_feature_InlineArrayTypeSugar

let _: [3 x Int]
let _ = [3 x Int](repeating: 0)
let _ = [3 x [3 x Int]](repeating: [1, 2, 3])

let _ = [[3 x Int] x Int]() // expected-error {{cannot pass type '[3 x Int]' as a value for generic value 'count'}}

do {
  let _: [3 x
  // expected-error@-1 {{expected type}}
}

do {
  let _: [3 x Int // expected-note {{to match this opening '['}}
} // expected-error {{expected ']' in inline array type}}

// We don't currently allow multi-line.
func testMultiline(_ x: Int) {
  let _ = [ // expected-error {{cannot call value of non-function type '[Int]'}}
    3 // expected-error {{expected ',' separator}}
    x
    Int
  ](repeating: 0)

  let _ = [3
    x
    Int
  ](repeating: 0)
  // expected-error@-4 {{expected ',' separator}}
  // expected-error@-5 {{cannot call value of non-function type '[Int]'}}

  let _ = [3
           x Int](repeating: 0)
  // expected-error@-2 {{cannot call value of non-function type '[Int]'}}
  // expected-error@-3 {{expected ',' separator}}

  // This is okay.
  let _ = [3 x
             Int](repeating: 0)

  // So's this
  let _ = [
    3 x Int
  ](repeating: 0)
}

protocol P {}
protocol Q {}

struct S<T> {}

let _ = S<[3 x Int]>()
let _ = S<[[3 x Int]]>()

// Make sure we can recover for different type productions as the LHS.
let _ = S<[[Int x 3]]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[@escaping () -> Int x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{'@escaping' may only be used in function parameter position}}
let _ = S<[Int.Type x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[sending P & Q x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{'sending' may only be used on parameters and results}}
let _ = S<[some P & Q -> Int x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{single argument function types require parentheses}}
// expected-error@-3 {{'some' types are only permitted in properties, subscripts, and functions}}
let _ = S<[~P x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{type 'P' cannot be suppressed}}
let _ = S<[(Int, String) x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[[3 x Int] x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[[Int] x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[Array<Int> x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[_ x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_? x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_?x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_! x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-warning@-2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
// expected-note@-3 {{use '?' instead}}
// expected-error@-4 {{could not infer type for placeholder}}
let _ = S<[_!x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-warning@-2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
// expected-note@-3 {{use '?' instead}}
// expected-error@-4 {{could not infer type for placeholder}}
let _ = S<[Int?x 3]>()
// expected-error@-1 {{element count must precede inline array element type}}

func testEllipsis(_ x: Int) {
  // Make sure this isn't parsed as '<variadic-type> x <missing type>'
  let _ = [x...x]
}
