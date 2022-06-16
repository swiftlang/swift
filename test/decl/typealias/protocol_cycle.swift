// RUN: %target-typecheck-verify-swift

// Make sure we don't get a cycle <T : P> => P.A => G => <T : P>.

func foo<T : P>(_: T) {}

protocol P {
  typealias A = G<Self>
}

struct G<T : P> {}
