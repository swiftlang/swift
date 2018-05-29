// RUN: %target-typecheck-verify-swift

#assert(true)

#assert(true, "error message")

#assert(false)

#assert(false, "error message")

#assert(123) // expected-error{{cannot convert value of type 'Int' to expected argument type 'Bool'}}

#assert(123, "error message") // expected-error{{cannot convert value of type 'Int' to expected argument type 'Bool'}}
