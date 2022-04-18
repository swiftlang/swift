// RUN: %target-typecheck-verify-swift

protocol P {
  typealias A = Array

  associatedtype X where X == A // expected-error {{reference to generic type 'Self.A' (aka 'Array') requires arguments in <...>}}
}
