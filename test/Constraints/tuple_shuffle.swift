// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 7 -verify-additional-prefix swift7-

// REQUIRES: swift7

func consume<T>(_ x: T) {} // Suppress unused variable warnings

func shuffle_through_initialization() {
  let a = (x: 1, y: 2)
  let b: (y: Int, x: Int)
  b = a
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from 'x:y:' to 'y:x:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-2 {{cannot implicitly reorder tuple elements from 'x:y:' to 'y:x:'}}
  consume(b)
}

func shuffle_raw_label(_ t: (`a b`: Int, `c d`: Int)) {
  let _: (`c d`: Int, `a b`: Int) = t
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from '`a b`:`c d`:' to '`c d`:`a b`:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-2 {{cannot implicitly reorder tuple elements from '`a b`:`c d`:' to '`c d`:`a b`:'}}
}

func shuffle_through_destructuring() {
  let a = (x: 1, y: 2)
  let (y: b, x: c) = a
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from 'x:y:' to 'y:x:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-2 {{cannot implicitly reorder tuple elements from 'x:y:' to 'y:x:'}}
  consume((b, c))
}

func shuffle_through_call() {
  func foo(_ : (x: Int, y: Int)) {}
  foo((y: 5, x: 10))
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from 'y:x:' to 'x:y:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-2 {{cannot implicitly reorder tuple elements from 'y:x:' to 'x:y:'}}
}

func shuffle_through_cast() {
  let x = ((a: Int(), b: Int()) as (b: Int, a: Int)).0
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from 'a:b:' to 'b:a:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-2 {{cannot implicitly reorder tuple elements from 'a:b:' to 'b:a:'}}

  // Ah, the famous double-shuffle
  let (c1, (c2, c3)): (c: Int, (b: Int, a: Int)) = ((a: Int(), b: Int()), c: Int())
  // expected-swift6-warning@-1 {{implicit reordering of tuple elements from 'a:b:' to 'b:a:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift6-warning@-2 {{implicit reordering of tuple elements from '_:c:' to 'c:_:' is deprecated; this will be an error in a future Swift language mode}}
  // expected-swift7-error@-3 {{cannot implicitly reorder tuple elements from 'a:b:' to 'b:a:'}}
  // expected-swift7-error@-4 {{cannot implicitly reorder tuple elements from '_:c:' to 'c:_:'}}
  consume((x, c1, c2, c3))
}
