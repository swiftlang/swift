// RUN: %target-typecheck-verify-swift

let _ = [
  #if FLAG
  "foo",
  #else
  "foo": 12, // expected-error {{expected ',' separator}} expected-error {{expected expression in container literal}}
  #endif
]

let _ = [ // expected-error {{empty collection literal requires an explicit type}}
#if true
] // expected-error {{expected #else or #endif at end of conditional compilation block}}
#endif // expected-error {{unexpected conditional compilation block terminator}}

let _ = [
#if true
  42 // expected-error {{expected ',' separator}}
#endif
]

let _ = [
#if true
  42 // expected-error {{expected ',' separator}}
#endif
  , // expected-error {{expected expression in container literal}}
  12,
]

let _ = [
  42,
#if true
  12, 2 // expected-error {{expected ',' separator}}
#endif
  0
]

let _ = [
#if true
  "foo": 42 // expected-error {{expected ',' separator}}
#endif
]

let _ = [
  1 // expected-error {{expected ',' separator}}
#if true
  2,
#endif
]
