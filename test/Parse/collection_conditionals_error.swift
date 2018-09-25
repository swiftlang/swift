// RUN: %target-typecheck-verify-swift

[
  #if FLAG
  "foo",
  #else
  "foo": 12,
  #endif
]
// expected-error@-3{{expected ',' separator}}
// expected-error@-4{{expected expression in container literal}}
// expected-warning@-9{{expression of type '[String]' is unused}}

[
#if true
]
#endif
// expected-error@-2{{expected expression in container literal}}
// expected-error@-2{{unexpected conditional compilation block terminator}}

[
#if true
  42
#endif
]
// expected-error@-3{{expected ',' separator}}
// expected-warning@-6{{expression of type '[Int]' is unused}}

[
#if true
  42
#endif
  ,
  12,
]
// expected-error@-5{{expected ',' separator}}
// expected-error@-4{{expected expression in container literal}}
// expected-warning@-9{{expression of type '[Int]' is unused}}

[
#if false
#else
  "foo": 42,
#endif
]
// expected-error@-4{{expected expression in container literal}}
