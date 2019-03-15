// RUN: %target-typecheck-verify-swift

protocol P1 {
  call() -> Self
}
extension P1 {
  call() -> Self {
    return self
  }
}

struct Missing : P1 {}

struct S1 : P1 {
  call() -> S1 {
    return self
  }
}

let s1 = S1()
_ = s1()()()

protocol P2 {}
extension P2 {
  call() -> Self {
    return self
  }
}
struct S2 : P2 {}

let s2 = S2()
// TODO: Fix this by fixing `getCallDeclarations` logic in CSSimplify.cpp.
// Need to handle overriding declarations; mimic/unify code with `lookupQualified`.
// _ = s2()()()
