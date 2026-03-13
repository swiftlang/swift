// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200

// https://github.com/swiftlang/swift/issues/46420

func slow() {
  let new_M: [Int] = Array<Int>(repeating: 0, count: 200)
  let _ = (0 ..< 32).map({ (a) -> Int in
      return new_M[a >> 3] >> ((1 ^ a & 7) * 4) & 15
  })
}
