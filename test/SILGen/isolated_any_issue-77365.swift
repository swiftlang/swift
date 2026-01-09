// RUN: %target-swift-emit-silgen %s -swift-version 6 -sil-verify-none

// REQUIRES: concurrency
// REQUIRES: no_asserts

// XFAIL: *

// FIXME: This sort of pattern should be diagnosed as an error in Sema if possible.
// For now, we expect an ASSERT in SILGen to prevent miscompiling.

// https://github.com/swiftlang/swift/issues/77365

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
