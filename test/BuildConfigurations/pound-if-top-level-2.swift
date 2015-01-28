// RUN: %target-parse-verify-swift

// expected-error@+1{{unexpected configuration expression type}}
#if 6
var u: V { didSet {} }

// expected-error@+1{{expected #else or #endif at end of configuration block}}
