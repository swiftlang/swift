// RUN: %target-typecheck-verify-swift -solver-scope-threshold=110000

func slow() {
  print(Array(1...5).filter({ $0 < 3 }).map({ $0 * 10 }).reduce(0, +))
}
