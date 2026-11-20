// rdar://175041208 (https://github.com/swiftlang/swift/issues/88540)
// RUN: %target-typecheck-verify-swift

func f2(_ a: Array<Double>) -> Double {
  fatalError()
}

func f2(_ a: Array<Int>) -> Int {
  f2( // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    a.map { Double($0) }
  )
}
