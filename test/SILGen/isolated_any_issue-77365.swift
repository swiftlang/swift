// RUN: not --crash %target-swift-emit-silgen %s -swift-version 5 -sil-verify-none 2>&1 | %FileCheck %s
// RUN: not --crash %target-swift-emit-silgen %s -swift-version 6 -sil-verify-none 2>&1 | %FileCheck %s

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/77365
// FIXME: This sort of pattern should be diagnosed as an error in Sema if possible.
// For now, we expect an ASSERT in SILGen to prevent miscompiling.

// CHECK: Assertion failed: {{.*}} function createInitExistentialRef

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
