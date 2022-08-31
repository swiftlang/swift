// RUN: %target-swift-frontend -emit-parse %s
// RUN: %target-swift-frontend -dump-parse %s

// Also makes sure -emit-parse and -dump-parse are both valid.

// Make sure we don't do any Sema and don't crash.
extension X {
  typealias Y = Z
}
