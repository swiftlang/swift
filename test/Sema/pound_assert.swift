// RUN: %target-typecheck-verify-swift -enable-experimental-static-assert

#assert(true)

#assert(true, "error message")

#assert(false)

#assert(false, "error message")

#assert(123) // expected-error{{'Int' is not convertible to 'Bool'}}

#assert(123, "error message") // expected-error{{'Int' is not convertible to 'Bool'}}
