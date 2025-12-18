// RUN: %target-swift-frontend -typecheck %s

extension Optional {
  init?() {
    self.init(nilLiteral: Void())
  }
}
