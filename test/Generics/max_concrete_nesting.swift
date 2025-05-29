// RUN: %target-typecheck-verify-swift -requirement-machine-max-concrete-nesting=4
// RUN: not %target-typecheck-verify-swift -requirement-machine-max-concrete-nesting=3

struct G<T> {}

// The nesting limit is relative to the initial set of rules, so here we
// can form a type with nesting depth 8.

func f<T, U>(_: T, _: U)
  where T: Sequence,
        U: Sequence,
        T.Element == G<G<G<G<U.Element>>>>,
        U.Element == G<G<G<G<T>>>> {}
