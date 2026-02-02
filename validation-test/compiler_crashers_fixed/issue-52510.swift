// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/52510
// Just make sure we don't crash.

enum Crash: String {
  case foo
  case bar(String)
    
  static let shared = Crash()
}

extension Crash {
  init() { self = .foo }
}
