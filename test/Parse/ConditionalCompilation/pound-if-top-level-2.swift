// RUN: %target-typecheck-verify-swift

#if 0  // expected-error {{'0' is not a valid conditional compilation expression, use 'false'}}
// expected-note@-1{{replace with Boolean literal 'false'}}{{5-6=false}}
  let x = 1
#endif

#if 1  // expected-error {{'1' is not a valid conditional compilation expression, use 'true'}}
// expected-note@-1{{replace with Boolean literal 'true'}}{{5-6=true}}
  let x = 1
#endif


// expected-error@+1{{invalid conditional compilation expression}}
#if 6
var u: V { didSet {} }

// expected-error@+1{{expected #else or #endif at end of conditional compilation block}}
