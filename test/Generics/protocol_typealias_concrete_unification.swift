// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on

protocol P1 {
  typealias T = Array<U>
  associatedtype U
}

protocol P2 {
  typealias T = Array<Int>
}

func foo<T : P1 & P2>(_: T, u: T.U) -> Int { return u }
