// RUN: %target-swift-emit-sil %s -swift-version 5
// RUN: %target-swift-emit-sil %s -swift-version 6

// REQUIRES: concurrency
// REQUIRES: no_asserts

actor A {
  func f() {
    let _: @isolated(any) () -> Void = { [unowned self] in
        self.preconditionIsolated()
    }
  }

  func g() {
    Task {
        [unowned self] in
        self.preconditionIsolated()
    }
  }
}
