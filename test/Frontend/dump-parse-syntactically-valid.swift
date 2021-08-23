// RUN: %target-swift-frontend -dump-parse %s

// Make sure we don't do any Sema and don't crash.
extension X {
  typealias Y = Z
}
