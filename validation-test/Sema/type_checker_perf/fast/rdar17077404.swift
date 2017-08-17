// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

func memoize<A: Hashable, R>(
  f: @escaping ((A) -> R, A) -> R
) -> ((A) -> R) {
  var memo = Dictionary<A,R>()

  var recur: ((A) -> R)!
  recur = { (a: A) -> R in
    if let r = memo[a] { return r }
    let r = f(recur, a)
    memo[a] = r
    return r
  }

  return recur
}

let fibonacci = memoize {
  (fibonacci, n) in
  n < 2 ? n as Int : fibonacci(n - 1) + fibonacci(n - 2)
}
