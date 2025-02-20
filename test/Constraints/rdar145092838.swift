// RUN: %target-typecheck-verify-swift

func f(a: Array<Int>, n: Int) {
  let _: Array = a.prefix(n) + a.suffix(a.count - n - 1)
}
