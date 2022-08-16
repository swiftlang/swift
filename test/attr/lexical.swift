// RUN: %target-typecheck-verify-swift

struct S {}

class C {}

enum E {}

@_eagerMove // expected-error {{'@_eagerMove' attribute cannot be applied to this declaration}}
typealias EagerTuple = (C, C)

func foo(@_lexical @_eagerMove _ e: E) {} // expected-error {{@_eagerMove and @_lexical attributes are alternate styles of lifetimes and can't be combined}}

func bar(@_lexical _ s: S) {} // okay

func baz(@_eagerMove _ c: C) {} // okay

struct S2 {
  @_eagerMove let c: C // okay
  @_lexical let e: E // okay
}

func foo() {
  @_lexical let s1 = S()
  @_eagerMove var s2 = S()
  @_lexical @_eagerMove let s3 = S() // expected-error {{@_eagerMove and @_lexical attributes are alternate styles of lifetimes and can't be combined}}
  _ = s1
  s2 = S()
  _ = s2
  _ = s3
}
