// Tiny test that only accesses specific members of the 'User' class, never
// forcing /all/ members to be loaded, in order to test recovery for lazy member
// loading.

import Typedefs
import Lib

func test(user: User) {
  _ = user.returnsWrappedMethod() // expected-error {{value of type 'User' has no member 'returnsWrappedMethod'; did you mean 'returnsUnwrappedMethod'?}}
  _ = user.returnsUnwrappedMethod() // okay
}
