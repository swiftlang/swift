// RUN: %target-parse-verify-swift

#if 0  // expected-error {{'0' is not a valid configuration option, use 'false'}}
  let x = 1
#endif

#if 1  // expected-error {{'1' is not a valid configuration option, use 'true'}}
  let x = 1
#endif


// expected-error@+1{{unexpected configuration expression type}}
#if 6
var u: V { didSet {} }

// expected-error@+1{{expected #else or #endif at end of configuration block}}
