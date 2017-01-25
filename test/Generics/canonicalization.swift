// RUN: %target-typecheck-verify-swift

// rdar://problem/23149063
protocol P0 { }

protocol P {
  associatedtype A
}

protocol Q : P {
  associatedtype A
}

func f<T>(t: T) where T : P, T : Q, T.A : P0 { } // expected-note{{'f(t:)' previously declared here}}
func f<T>(t: T) where T : Q, T : P, T.A : P0 { } // expected-error{{invalid redeclaration of 'f(t:)'}}