// RUN: %target-typecheck-verify-swift -solver-scope-threshold=300 -solver-enable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=600 -solver-disable-promote-supertypes

func slow() {
  print(Array(1...5).filter({ $0 < 3 }).map({ $0 * 10 }).reduce(0, +))
}
