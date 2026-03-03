// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype A

  func f(_: A)
}

protocol P2: P1 {
  associatedtype A = Int
}

func foo<T: P1>(_: T.Type) -> T.A.Type {}

_ = foo(S.self)

struct S: P2 {
  func f(_: A) {}
}
