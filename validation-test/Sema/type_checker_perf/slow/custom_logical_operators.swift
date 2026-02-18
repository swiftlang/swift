// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan
// REQUIRES: objc_interop

// https://forums.swift.org/t/typechecker-performance-is-way-worse-in-6-3/84779

import Combine

enum E {
  case active
}

public struct AndPredicate<T> {}
public struct OrPredicate<T> {}

public func && <Predicate> (_ lhs: Predicate, _ rhs: Predicate) -> AndPredicate<Predicate> {
  fatalError()
}

public func || <Predicate> (_ lhs: Predicate, _ rhs: Predicate) -> OrPredicate<Predicate> {
  fatalError()
}

func f<T: Publisher, U: Publisher, V: Publisher>(_ sendingAction: T, _ conversationState: U, _ shouldDisableUserInput: V)
  where T.Failure == U.Failure, U.Failure == V.Failure, T.Output == Bool, U.Output == E, V.Output == Bool {
  // expected-error@+1 {{reasonable time}}
  let _ = Publishers.CombineLatest3(sendingAction, conversationState, shouldDisableUserInput)
    .map { sendingAction, state, disableInput in
      sendingAction == false && state == .active && disableInput == false
    }
    .removeDuplicates()
    .eraseToAnyPublisher()
}
