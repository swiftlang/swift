// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/85587
do {
  struct Mootex<T> {
    func withLock<Result>(_: (T) -> Result) -> Result {}
  }

  func never() -> Never {}

  struct Test {
    let _i = Mootex<Int>()

    var i: Int {
      _i.withLock { _ in
        never()
      }
    }
  }
}
