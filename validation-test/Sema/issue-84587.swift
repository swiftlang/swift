// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/84587

protocol P {}
struct K: P {}

struct S<T>: P {
  init<U>(_: U?, _: T = K()) {}
}

extension P {
  func bar<T>(of x: T, _ fn: (T) -> Void) -> some P {
    K()
  }
}

func foo(_ x: Int) {
  _ = S(true ? x : nil).bar(of: x) { _ in
    Task {}
  }
}
