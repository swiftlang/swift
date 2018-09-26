// RUN: %target-typecheck-verify-swift

[ // expected-warning {{expression of type '[String]' is unused}}
  #if FLAG
  "foo",
  #else
  "foo": 12, // expected-error {{expected ',' separator}}
  // expected-error@-1{{expected expression in container literal}}
  #endif
]

[ // expected-warning {{expression of type '[String : Int]' is unused}}
#if FLAG
  // FIXME: commenting out until blah blah...
  /* "foo": 1000, */
#else
  "bar": 42,
#endif
  "baz": 0,
]

[
#if true
] // expected-error {{expected expression in container literal}}
#endif // expected-error {{unexpected conditional compilation block terminator}}

[ // expected-warning {{expression of type '[Int]' is unused}}
#if true
  42 // expected-error {{expected ',' separator}}
#endif
]

[ // expected-warning {{expression of type '[Int]' is unused}}
#if true
  42 // expected-error {{expected ',' separator}}
#endif
  , // expected-error {{expected expression in container literal}}
  12,
]

_ = [
  42,
#if true
  12, 2 // expected-error {{expected ',' separator}}
#endif
  0
]

_ = [
#if true
  "foo": 42 // expected-error {{expected ',' separator}}
#endif
]
