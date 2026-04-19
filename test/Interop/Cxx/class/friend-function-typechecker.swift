// RUN: %target-swiftxx-frontend -typecheck -verify -suppress-notes -I %S/Inputs %s

import FriendFunction

func f(a: A, b: B, c: C) {
  a.memberInA(42)
  b.memberInA(42) // expected-error {{has no member}}
  c.memberInA(42) // expected-error {{has no member}}

  a.memberInB() // expected-error {{has no member}}
  b.memberInB()
  c.memberInB()

  a.memberInC() // expected-error {{has no member}}
  b.memberInC() // expected-error {{has no member}}
  c.memberInC()
}
