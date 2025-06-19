// RUN: %target-typecheck-verify-swift -enable-experimental-feature InlineArrayTypeSugar -disable-availability-checking

// REQUIRES: swift_feature_InlineArrayTypeSugar

let _: [3 of Int]
let _ = [3 of Int](repeating: 0)
let _ = [3 of [3 of Int]](repeating: [1, 2, 3])

let _ = [[3 of Int] of Int]() // expected-error {{cannot pass type '[3 of Int]' as a value for generic value 'count'}}

do {
  let _: [3 of
  // expected-error@-1 {{expected type}}
}

do {
  let _: [3 of Int // expected-note {{to match this opening '['}}
} // expected-error {{expected ']' in inline array type}}

// We don't currently allow multi-line.
func testMultiline(_ of: Int) {
  let _ = [ // expected-error {{cannot call value of non-function type '[Int]'}}
    3 // expected-error {{expected ',' separator}}
    of
    Int
  ](repeating: 0)

  let _ = [3
    of
    Int
  ](repeating: 0)
  // expected-error@-4 {{expected ',' separator}}
  // expected-error@-5 {{cannot call value of non-function type '[Int]'}}

  let _ = [3
           of Int](repeating: 0)
  // expected-error@-2 {{cannot call value of non-function type '[Int]'}}
  // expected-error@-3 {{expected ',' separator}}

  // This is okay.
  let _ = [3 of
             Int](repeating: 0)

  // So's this
  let _ = [
    3 of Int
  ](repeating: 0)
}

protocol P {}
protocol Q {}

struct S<T> {}

let _ = S<[3 of Int]>()
let _ = S<[[3 of Int]]>()

// Make sure we can recover for different type productions as the LHS.
let _ = S<[[Int of 3]]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[@escaping () -> Int of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{'@escaping' may only be used in function parameter position}}
let _ = S<[Int.Type of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[sending P & Q of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{'sending' may only be used on parameters and results}}
let _ = S<[some P & Q -> Int of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{single argument function types require parentheses}}
// expected-error@-3 {{'some' types are only permitted in properties, subscripts, and functions}}
let _ = S<[~P of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{type 'P' cannot be suppressed}}
let _ = S<[(Int, String) of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[[3 of Int] of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[[Int] of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[Array<Int> of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
let _ = S<[_ of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_? of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_?of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-error@-2 {{could not infer type for placeholder}}
let _ = S<[_! of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-warning@-2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
// expected-note@-3 {{use '?' instead}}
// expected-error@-4 {{could not infer type for placeholder}}
let _ = S<[_!of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}
// expected-warning@-2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
// expected-note@-3 {{use '?' instead}}
// expected-error@-4 {{could not infer type for placeholder}}
let _ = S<[Int?of 3]>()
// expected-error@-1 {{element count must precede inline array element type}}

func testEllipsis(_ of: Int) {
  // Make sure this isn't parsed as '<variadic-type> of <missing type>'
  let _ = [of...of]
}
