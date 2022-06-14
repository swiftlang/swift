// RUN: %target-swift-frontend -typecheck %s

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
extension Actor {
  func f() { }

  func g(a: [Int]) {
    a.forEach { i in
      f()
    }
  }
}
