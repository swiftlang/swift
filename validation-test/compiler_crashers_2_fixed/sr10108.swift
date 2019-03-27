// RUN: not %target-swift-frontend -typecheck %s

// Just make sure we don't crash.

enum Crash: String {
  case foo
  case bar(String)
    
  static let shared = Crash()
}

extension Crash {
  init() { self = .foo }
}
