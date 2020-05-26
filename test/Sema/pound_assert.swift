// RUN: %target-typecheck-verify-swift -enable-experimental-static-assert

#assert(true)

#assert(true, "error message")

#assert(false)

#assert(false, "error message")

#assert(123) // expected-error{{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}

#assert(123, "error message") // expected-error{{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
