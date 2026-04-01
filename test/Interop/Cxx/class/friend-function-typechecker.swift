// RUN: %target-swiftxx-frontend -typecheck -verify -I %S/Inputs %s

// expected-no-diagnostics

// Ensure that a friend function declared inside a nested type does not get
// incorrectly imported as a member of the enclosing namespace.

import FriendFunction

extension NS {
  func foo(_ code: (NS.MyString) -> ()) {
    let _ = makeMyString()
  }
}

func bar() {
  let _ = makeMyString()
}
