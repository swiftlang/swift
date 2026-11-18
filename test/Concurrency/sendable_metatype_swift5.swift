// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

protocol P {
  static func doSomething()
}

func doSomethingStatic<T: P>(_: T.Type) {
  Task { @concurrent in
    T.doSomething()
  }
}
